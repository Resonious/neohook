import neohook/counter
import gleam/string_tree
import gleam/bit_array
import parrot/dev
import gleam/dynamic/decode
import gleam/json
import neohook/sql
import neohook/http_wrapper.{type Request, type Response}
import pturso
import migrations
import gulid.{type Ulid}
import gleam/dynamic
import gleam/list
import gleam/order
import gleam/bool.{lazy_guard}
import gleam/int
import termcolor
import gleam/uri
import env
import tls
import gleam/dict
import gleam/string
import pipemaster
import pipe
import gleam/http
import gleam/result
import gleam/option.{None, Some}
import gleam/erlang/process.{type Pid}
import gleam/erlang/atom.{type Atom}
import gleam/http/request
import logging
import gleam/http/response
import gleam/bytes_tree
import gleam/otp/actor
import mist

@external(erlang, "hot", "make_handler")
fn make_handler(state: AppState) -> fn(request.Request(mist.Connection)) -> Response

@external(erlang, "hot", "db_receive_loop")
fn hot_db_receive_loop(peer: Peer, db: pturso.Connection) -> Nil

@external(erlang, "hot", "db_send_loop")
fn hot_db_send_loop(db: pturso.Connection, peers: List(Peer), me: Peer) -> Nil

type SseState {
  SseState(
    id: Int,
    pipe_name: String,
    master: pipemaster.Subject,
  )
}

/// Same as uri.to_string but doesn't include the port when it's the
/// default for the protocol (80, 443).
/// (also has termcolors)
pub fn colorize_uri(uri: uri.Uri) -> String {
  let parts = case uri.fragment {
    Some(fragment) -> ["#", fragment]
    None -> []
  }
  let parts = case uri.query {
    Some(query) -> ["?", query, ..parts]
    None -> parts
  }
  let parts = [termcolor.red, uri.path, termcolor.reset, ..parts]
  let parts = case uri.host, string.starts_with(uri.path, "/") {
    Some(host), False if host != "" -> ["/", ..parts]
    _, _ -> parts
  }
  let parts = case uri.host, uri.port {
    _, Some(80) | _, Some(443) -> parts
    Some(_), Some(port) -> [":", int.to_string(port), ..parts]
    _, _ -> parts
  }
  let parts = case uri.scheme, uri.userinfo, uri.host {
    Some(s), Some(u), Some(h) -> [s, "://", u, "@", h, ..parts]
    Some(s), None, Some(h) -> [s, "://", h, ..parts]
    Some(s), Some(_), None | Some(s), None, None -> [s, ":", ..parts]
    None, None, Some(h) -> ["//", h, ..parts]
    _, _, _ -> parts
  }
  string.concat(parts)
}

type CurlState {
  CurlState(
    id: Int,
    pipe_name: String,
    master: pipemaster.Subject,
  )
}

fn listen_on_pipe_for_curl(
  req: Request,
  pipe_name: String,
  master: pipemaster.Subject,
) -> Response {
  let colorized_url = req |> request.to_uri |> colorize_uri

  let welcome_message =
    bytes_tree.new()
    |> bytes_tree.append_string("You are listening on ")
    |> bytes_tree.append_string(termcolor.red)
    |> bytes_tree.append_string("/")
    |> bytes_tree.append_string(pipe_name)
    |> bytes_tree.append_string(termcolor.reset)
    |> bytes_tree.append_string("\n\n")
    |> bytes_tree.append_string("Try this in another terminal: ")
    |> bytes_tree.append_string("\n  curl ")
    |> bytes_tree.append_string(termcolor.cyan)
    |> bytes_tree.append_string("-d ")
    |> bytes_tree.append_string(termcolor.yellow)
    |> bytes_tree.append_string("'Hello, World!' ")
    |> bytes_tree.append_string(termcolor.cyan)
    |> bytes_tree.append_string(colorized_url)
    |> bytes_tree.append_string(termcolor.reset)
    |> bytes_tree.append_string("\n\n")
    |> bytes_tree.to_bit_array

  http_wrapper.chunked(
    req,
    response.new(200) |> response.set_header("content-type", "application/octet-stream"),
    init: fn(receiver) {
      // Create a pipe actor that transforms pipe.Message -> BitArray
      let pid = process.self()
      let assert Ok(pipe_started) = pipe.new(pipe.Curl(receiver, pid))
      let pipe_subj = pipe_started.data

      // Register the pipe actor with pipemaster
      let id = pipemaster.new_pipe_id()
      process.send(master, pipemaster.AddPipe(
        name: pipe_name,
        id: id,
        subject: pipe_subj,
      ))

      // Send welcome message
      process.send(receiver, welcome_message)

      CurlState(id, pipe_name, master)
    },
    loop: fn(state, data, conn) {
      case http_wrapper.send_chunk(conn, data) {
        Ok(_) -> http_wrapper.chunk_continue(state)
        Error(_) -> {
          process.send(state.master, pipemaster.CleanPipe(state.pipe_name, remove_id: state.id))
          http_wrapper.chunk_stop()
        }
      }
    },
  )
}

fn listen_on_pipe_for_sse(
  req: Request,
  pipe_name: String,
  master: pipemaster.Subject,
) -> Response {
  http_wrapper.server_sent_events(
    req,
    response.new(200) |> response.set_header("content-type", "text/event-stream"),
    init: fn(subject) {
      let id = pipemaster.new_pipe_id()
      process.send(master, pipemaster.AddPipe(
        name: pipe_name,
        id: id,
        subject: subject,
      ))
      SseState(id, pipe_name, master)
    },
    loop: fn(state, message, conn) {
      case pipe.handle(pipe.Sse(conn), message) {
        pipe.Dead -> {
          process.send(state.master, pipemaster.CleanPipe(state.pipe_name, remove_id: state.id))
          actor.stop()
        }
        _ -> actor.continue(state)
      }
    },
  )
}

@external(erlang, "atoms", "pipe_receiver")
fn pipe_entry_receiver_name() -> Atom

@external(erlang, "atoms", "db_sync_receiver")
fn db_sync_receiver_name() -> Atom

fn globally_configured_erlang_peers() -> List(Peer) {
  env.get("ERLANG_PEERS")
  |> option.map(string.split(_, ","))
  |> option.lazy_unwrap(fn() { [] })
  |> list.map(atom.create)
  |> list.map(fn(address) {
    RemotePeer(
      address:,
      pipe_entry_name: pipe_entry_receiver_name(),
      db_sync_name: db_sync_receiver_name(),
    )
  })
}

pub fn my_erlang_node_id() -> Atom {
  let ip = env.get("ERLANG_NODE_IP") |> option.unwrap("127.0.0.1")
  atom.create("neohook@" <> ip)
}

@external(erlang, "erlang", "send")
fn send_to_remote(destination: #(Atom, Atom), message: a) -> a

fn peer_send_pipe_entry(peer: Peer, message: pipemaster.Message) {
  case peer {
    RemotePeer(address:, pipe_entry_name:, ..) -> {
      send_to_remote(#(pipe_entry_name, address), message)
      Nil
    }

    LocalPeer(pipe_entry_subj:, ..) -> {
      process.send(pipe_entry_subj, message)
    }
  }
}

fn peer_send_db_sync(peer: Peer, message: DbSyncMessage) {
  case peer {
    RemotePeer(address:, db_sync_name:, ..) -> {
      send_to_remote(#(db_sync_name, address), message)
      Nil
    }

    LocalPeer(db_sync_subj:, ..) -> {
      process.send(db_sync_subj, message)
    }
  }
}

fn parrot_to_pturso(p: dev.Param) -> pturso.Param {
  case p {
    dev.ParamInt(x) -> pturso.Int(x)
    dev.ParamString(x) -> pturso.String(x)
    dev.ParamFloat(x) -> pturso.Float(x)
    dev.ParamBool(True) -> pturso.Int(1)
    dev.ParamBool(False) -> pturso.Int(0)
    dev.ParamBitArray(x) -> pturso.Blob(x)
    dev.ParamTimestamp(_) -> panic as "not supported"
    dev.ParamDate(_) -> panic as "not supported"
    dev.ParamList(_) -> panic as "not supported"
    dev.ParamDynamic(_) -> panic as "not supported"
    dev.ParamNullable(Some(x)) -> parrot_to_pturso(x)
    dev.ParamNullable(None) -> pturso.Null
  }
}

pub type Peer {
  LocalPeer(
    name: String,
    pipe_entry_subj: pipemaster.Subject,
    db_sync_subj: process.Subject(DbSyncMessage),
  )
  RemotePeer(
    address: Atom,
    pipe_entry_name: Atom,
    db_sync_name: Atom,
  )
}

pub fn peer_node(peer: Peer) {
  case peer {
    LocalPeer(name:, ..) -> name
    RemotePeer(address:, ..) -> atom.to_string(address)
  }
}

fn sender_of_request(req: Request) -> option.Option(String) {
  // TODO: fetch from auth header!!
  case http_wrapper.ip_address(of: req) {
    Ok(ip) -> Some("ip|" <> ip)
    Error(Nil) -> None
  }
}

fn send_to_pipe(
  req: Request,
  pipe_name: String,
  state: AppState,
) -> Response {
  // TODO: body size should be based on user
  case http_wrapper.read_body(req, 1024 * 100 * 50) {
    Ok(request.Request(body:, ..)) -> {
      let id = gulid.new()
      let id_string = state.ulid_to_string(id)
      let message = pipemaster.PushEntry(
        pipe_name,
        pipe.Entry(
          id: gulid.to_bitarray(id),
          method: req.method,
          headers: [#("x-snd-id", id_string), ..req.headers],
          body:,
          sender: sender_of_request(req),
        )
      )
      let namespace = pipe_namespace(of: pipe_name)

      // This sends the entry to all active listeners.
      process.send(state.master, message)

      let headers_json = message.entry.headers
        |> list.map(fn(x) { #(x.0, json.string(x.1)) })
        |> json.object
        |> json.to_string

      // The insert query checks the `persisted` flag inline, and
      // returns col_0=1 when the insert actually happened.
      let #(sql, with, expecting) = sql.insert_pipe_entry(
        id: message.entry.id,
        node: peer_node(state.self),
        pipe: pipe_name,
        namespace:,
        method: Some(message.entry.method |> http.method_to_string),
        headers: Some(headers_json),
        body: Some(message.entry.body),
        sender: message.entry.sender,
      )
      let with = with |> list.map(parrot_to_pturso)
      case pturso.query(sql, on: state.db, with:, expecting:) {
        // This means the pipe *was* marked as persisted, and the insert
        // was successful, so we need to tell peers about the new data.
        Ok([sql.InsertPipeEntry(col_0: 1)]) -> 
          state.peers |> list.each(fn(peer) {
            logging.log(logging.Info, "sending to " <> peer_node(peer))
            peer_send_db_sync(peer, DbSyncNewEntry(who: state.self))
          })

        // Something went horribly wrong here
        Error(error) -> { echo error Nil }

        // No insert was performed
        _ -> Nil
      }

      // Always tell peers to forward the pipe entry to active listeners.
      state.peers |> list.each(fn(peer) {
        logging.log(logging.Info, "sending to " <> peer_node(peer))
        peer_send_pipe_entry(peer, message)
      })

      let response_body = bytes_tree.from_string(id_string)
        |> bytes_tree.append(<<"\n":utf8>>)

      response.new(200)
      |> response.set_header("content-type", "text/plain")
      |> response.set_body(mist.Bytes(response_body))
    }
    Error(mist.ExcessBody) ->
      response.new(413)
      |> response.set_header("content-type", "text/plain; charset=utf-8")
      |> response.set_body(mist.Bytes(bytes_tree.from_string("Body too large")))
    Error(mist.MalformedBody) ->
      response.new(400)
      |> response.set_header("content-type", "text/plain; charset=utf-8")
      |> response.set_body(mist.Bytes(bytes_tree.from_string("Invalid request")))
  }
}

fn serve_html(path: String, status status: Int) -> Response {
  let assert Ok(file) = mist.send_file(path, offset: 0, limit: None)

  response.new(status)
  |> response.set_header("content-type", "text/html; charset=utf-8")
  |> response.set_body(file)
}

fn serve_static(path: String, content_type: String) {
  let assert Ok(file) = mist.send_file(path, offset: 0, limit: None)

  response.new(200)
  |> response.set_header("content-type", content_type)
  |> response.set_body(file)
}

fn remove_repeat_slashes(from str: String) -> String {
  let scrubbed = string.replace(str, "//", "/")
  case string.compare(str, scrubbed) {
    order.Eq -> str
    _ -> remove_repeat_slashes(from: scrubbed)
  }
}

/// Returns a function so that it can be used nicely with lazy_guard
fn redirect(to location: String) {
  fn() {
    response.new(301)
    |> response.set_header("location", location)
    |> response.set_body(mist.Bytes(bytes_tree.new()))
  }
}

/// Returns a function so that it can be used nicely with lazy_guard
fn bad_request(because reason: String) {
  fn() {
    response.new(301)
    |> response.set_header("content-type", "application/json")
    |> response.set_body(mist.Bytes(
      json.object([#("error", json.string(reason))])
      |> json.to_string_tree
      |> bytes_tree.from_string_tree
    ))
  }
}

fn log_request(req: Request, return: fn() -> Response) -> Response {
  let method_str = http.method_to_string(req.method)
  let user_agent = request.get_header(req, "user-agent")
    |> result.unwrap("unknown")

  let resp = return()
  let resp_status = resp.status |> int.to_string

  logging.log(
    logging.Info,
    method_str <> " " <> req.path <> " -> " <> resp_status
    <> " (" <> user_agent <> ")"
  )

  resp
}

pub type AppState {
  AppState(
    master: pipemaster.Subject,
    ulid_to_string: fn(Ulid) -> String,
    ulid_from_string: fn(String) -> Result(Ulid, gulid.UlidError),
    db: pturso.Connection,
    self: Peer,
    peers: List(Peer),
  )
}

/// This is called by make_handler in Erlang code.
/// Doing so allows us to support hot reload, so we can update handler logic
/// without worrying about shutting down the server.
pub fn http_handler_for_mist(req: request.Request(mist.Connection), state: AppState) -> Response {
  let request.Request(body: conn, ..) = req
  let req = http_wrapper.convert_request(req, http_wrapper.MistBody(conn))
  http_handler(req, state)
}

fn serve_webpage(path: List(String), _req: Request, _state: AppState) -> Response {
  case path {
    ["account"] -> serve_html("static/account.html", status: 200)
    _ -> serve_html("static/404.html", status: 404)
  }
}

fn serve_api(api_path: List(String), req: Request, state: AppState) -> Response {
  case req.method, api_path {
    http.Get, ["pipe"] -> {
      let pipe_key = "name"
      let pipe = req.query
        |> option.unwrap("")
        |> uri.parse_query
        |> result.unwrap([])
        |> list.key_find(pipe_key)

      use pipe <- expect(pipe, or_return: bad_request(because: "missing `?"<>pipe_key<>"=...`"))
      let namespace = pipe_namespace(of: pipe)

      let entries_subj = process.new_subject()
      process.spawn(fn() {
        let #(sql, with, expecting) = sql.pipe_entries_by_pipe(pipe:, limit: 10, offset: 0)
        let with = with |> list.map(parrot_to_pturso)

        let render_body = fn(body: BitArray) -> String {
          case bit_array.to_string(body) {
            Ok(x) -> x
            Error(_) -> bit_array.base64_encode(body, True)
          }
        }

        pturso.query(sql, on: state.db, with:, expecting:)
        |> result.unwrap([])
        |> json.array(fn(e) {
          let id = e.id |> gulid.from_bitarray |> result.lazy_unwrap(gulid.new)
          let #(time, _) = id |> gulid.to_parts
          json.object([
            #("id", id |> state.ulid_to_string |> json.string),
            #("timestamp", json.int(time)),
            #("headers", json.parse(
              e.headers |> option.unwrap("{}"),
              decode.dict(decode.string, decode.string)
            )
            |> result.lazy_unwrap(dict.new)
            |> dict.to_list
            |> list.map(fn(x) { #(x.0, json.string(x.1)) })
            |> json.object
            ),
            #("body", json.string(
              e.body
              |> option.lazy_unwrap(fn() { bit_array.from_string("") })
              |> render_body
            ))
          ])
        })
        |> process.send(entries_subj, _)
      })

      let settings_subj = process.new_subject()
      process.spawn(fn() {
        let #(sql, with, expecting) = sql.latest_pipe_settings(namespace:)
        let with = with |> list.map(parrot_to_pturso)

        let value = pturso.query(sql, on: state.db, with:, expecting:)
          |> result.unwrap([])
          |> list.first

        let flags = value
          |> result.map(fn(x) { x.flags })
          |> result.map(pipe.parse_flags)
          |> result.lazy_unwrap(pipe.default_flags)

        process.send(settings_subj, pipe.flags_to_json(flags))
      })

      let entries = process.receive(entries_subj, within: 1000)
        |> result.lazy_unwrap(fn() { json.array([], of: fn(_) { json.null() }) })
      let settings = process.receive(settings_subj, within: 1000)
        |> result.unwrap(json.null())

      let payload = json.object([
        #("entries", entries),
        #("settings", settings),
      ])
      |> json.to_string_tree

      response.new(200)
      |> response.set_header("content-type", "application/json")
      |> response.set_body(mist.Bytes(bytes_tree.from_string_tree(payload)))
    }

    http.Post, ["pipe", "settings"] -> {
      let pipe_key = "pipe"
      let pipe = req.query
        |> option.unwrap("")
        |> uri.parse_query
        |> result.unwrap([])
        |> list.key_find(pipe_key)

      use pipe <- expect(pipe, or_return: bad_request(because: "missing `?"<>pipe_key<>"=...`"))
      let namespace = pipe_namespace(of: pipe)

      let body = http_wrapper.read_body(req, 1024 * 8)
      use <- lazy_guard(when: result.is_error(body), return: bad_request(because: "body too big"))
      let assert Ok(request.Request(body:, ..)) = body

      let new_flags = json.parse_bits(body, pipe.flags_update_decoder())
      use new_flags <- expect(new_flags, or_return: bad_request(because: "missing fields"))

      let #(sql, with, expecting) = sql.latest_pipe_settings(namespace:)
      let with = with |> list.map(parrot_to_pturso)

      let latest_settings = pturso.query(sql, on: state.db, with:, expecting:)
        |> result.unwrap([])
        |> list.first

      let current_flags = latest_settings
        |> result.map(fn(x) { x.flags })
        |> result.map(pipe.parse_flags)
        |> result.lazy_unwrap(pipe.default_flags)

      let new_flags = pipe.Flags(
        persisted: option.unwrap(new_flags.persisted, current_flags.persisted)
      )

      let id = gulid.new() |> gulid.to_bitarray

      let #(sql, with) = sql.insert_pipe_settings(
        id:,
        node: peer_node(state.self),
        namespace:,
        flags: pipe.serialize_flags(new_flags),
      )
      let with = with |> list.map(parrot_to_pturso)

      case pturso.query(sql, on: state.db, with:, expecting: decode.success(Nil)) {
        Ok(_) -> response.new(204)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.new()))
        Error(e) -> {
          echo e
          response.new(500)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string("database error")))
        }
      }
    }

    http.Post, ["accounts"] -> {
      let id = gulid.new()
      let updated_at = gulid.erl_system_time_millis()
      let #(sql, with) = sql.create_account(
        id: id |> gulid.to_bitarray,
        node: peer_node(state.self),
        updated_at: 
      )
      let with = with |> list.map(parrot_to_pturso)

      case pturso.query(sql, on: state.db, with:, expecting: decode.success(Nil)) {
        Ok(_) -> {
          let payload = json.object([
            #("id", id |> state.ulid_to_string |> json.string),
          ])
          |> json.to_string_tree

          response.new(201)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string_tree(payload)))
        }

        Error(e) -> {
          echo e
          response.new(500)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string("database error")))
        }
      }
    }

    http.Post, ["accounts", account_id, "keys"] -> {
      use account_id <- expect(
        state.ulid_from_string(account_id) |> result.map(gulid.to_bitarray),
        or_return: bad_request(because: "invalid account_id"),
      )

      let body = http_wrapper.read_body(req, 1024 * 8)
      use <- lazy_guard(when: result.is_error(body), return: bad_request(because: "body too big"))
      let assert Ok(request.Request(body:, ..)) = body

      let id = gulid.new()
      let updated_at = gulid.erl_system_time_millis()
      let #(sql, with) = sql.add_key_to_account(
        id: id |> gulid.to_bitarray,
        node: peer_node(state.self),
        updated_at:,
        account_id:,
        jwk: Some(body),
      )
      let with = with |> list.map(parrot_to_pturso)

      case pturso.query(sql, on: state.db, with:, expecting: decode.success(Nil)) {
        Ok(_) -> {
          let payload = json.object([
            #("id", id |> state.ulid_to_string |> json.string),
          ])
          |> json.to_string_tree

          response.new(201)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string_tree(payload)))
        }

        Error(e) -> {
          echo e
          response.new(500)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string("database error")))
        }
      }
    }

    http.Delete, ["accounts", account_id, "keys", key_id] -> {
      use account_id <- expect(
        state.ulid_from_string(account_id) |> result.map(gulid.to_bitarray),
        or_return: bad_request(because: "invalid account_id"),
      )
      use key_id <- expect(
        state.ulid_from_string(key_id) |> result.map(gulid.to_bitarray),
        or_return: bad_request(because: "invalid key_id"),
      )

      let updated_at = gulid.erl_system_time_millis()

      let #(sql, with) = sql.remove_key_from_account(
        id: key_id,
        node: peer_node(state.self),
        updated_at:,
        account_id:,
      )
      let with = with |> list.map(parrot_to_pturso)

      case pturso.query(sql, on: state.db, with:, expecting: decode.success(Nil)) {
        Ok(_) -> {
          response.new(204)
          |> response.set_body(mist.Bytes(bytes_tree.from_string("")))
        }

        Error(e) -> {
          echo e
          response.new(500)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string("database error")))
        }
      }
    }

    http.Get, ["accounts", account_id, "keys"] -> {
      use account_id <- expect(
        state.ulid_from_string(account_id) |> result.map(gulid.to_bitarray),
        or_return: bad_request(because: "invalid account_id"),
      )

      let #(sql, with, expecting) = sql.keys_for_account(account_id:)
      let with = with |> list.map(parrot_to_pturso)

      case pturso.query(sql, on: state.db, with:, expecting:) {
        Ok(keys) -> {
          let payload = json.object([
            #("keys", json.array(keys, fn(key) {
              let key_id = key.id
                |> gulid.from_bitarray
                |> result.map(state.ulid_to_string)
                |> result.unwrap("")
              let jwk = key.jwk
                |> option.map(bit_array.base64_encode(_, True))
                |> option.unwrap("")
              json.object([
                #("id", json.string(key_id)),
                #("jwk", json.string(jwk)),
              ])
            })),
          ])
          |> json.to_string_tree

          response.new(200)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string_tree(payload)))
        }

        Error(e) -> {
          echo e
          response.new(500)
          |> response.set_header("content-type", "application/json")
          |> response.set_body(mist.Bytes(bytes_tree.from_string("database error")))
        }
      }
    }

    _, _ -> {
      response.new(404)
      |> response.set_header("content-type", "application/json")
      |> response.set_body(mist.Bytes(bytes_tree.from_string("{\"error\":\"path not found\"}")))
    }
  }
}

pub fn http_handler(req: Request, state: AppState) -> Response {
  use <- log_request(req)

  // It seems the "//" path results in an empty string path
  use <- lazy_guard(when: req.path == "", return: redirect(to: "/"))

  // Remove repeated slashes from paths
  let scrubbed_path = remove_repeat_slashes(from: req.path)
  use <- lazy_guard(when: req.path != scrubbed_path, return: redirect(to: scrubbed_path))

  // routing
  case request.path_segments(req) {
    // TODO: "/" needs to look better on curl!!
    [] -> serve_html("static/landing.html", status: 200)
    ["favicon.png"] -> serve_static("static/favicon.png", "image/png")
    ["favicon.svg"] -> serve_static("static/favicon.svg", "image/svg+xml")

    ["api", ..rest] -> serve_api(rest, req, state)
    ["_", ..rest] -> serve_webpage(rest, req, state)

    parts -> {
      let pipe_name = compute_pipe_name(parts)

      case req.method, infer_requester_type(from_headers: req.headers), pipe_name {
        http.Get, CurlRequester, Some(pipe_name) -> listen_on_pipe_for_curl(req, pipe_name, state.master)
        http.Get, SseRequester, Some(pipe_name) -> listen_on_pipe_for_sse(req, pipe_name, state.master)
        http.Get, UnknownRequester, Some(_) -> serve_html("static/view.html", status: 200)
        http.Post, _, Some(pipe_name) -> send_to_pipe(req, pipe_name, state)
        _, _, Some(_) ->
          response.new(405)
          |> response.set_header("content-type", "text/plain; charset=utf-8")
          |> response.set_body(mist.Bytes(bytes_tree.from_string("method not allowed")))

        _, UnknownRequester, None -> serve_html("static/404.html", status: 404)
        _, SseRequester, None -> serve_html("static/404.html", status: 404)
        _, CurlRequester, None -> serve_curl_404()
      }
    }
  }
}

fn serve_curl_404() {
  let body = bytes_tree.new()
    |> bytes_tree.append_string(termcolor.red)
    |> bytes_tree.append_string("404 NOT FOUND\n")
    |> bytes_tree.append_string(termcolor.reset)
    |> bytes_tree.append_string("\n")
    |> bytes_tree.append_string("This path isn't a valid pipe.\n")

  response.new(404)
  |> response.set_body(mist.Bytes(body))
}

type RequesterType {
  CurlRequester
  SseRequester
  UnknownRequester
}

fn infer_requester_type(from_headers headers: List(#(String, String))) -> RequesterType {
  let h = dict.from_list(headers)

  case dict.get(h, "accept"), dict.get(h, "user-agent") {
    Ok("text/event-stream"), _  -> SseRequester
    _, Ok("curl/" <> _)         -> CurlRequester
    _, _                        -> UnknownRequester
  }
}

/// Returns None if the pipe name is considered invalid
fn compute_pipe_name(parts: List(String)) -> option.Option(String) {
  let name = string.join(parts, "/")
  let is_invalid = string.is_empty(name) || string.contains(name, ".")

  case is_invalid {
    True -> option.None
    False -> option.Some(name)
  }
}

fn pipe_namespace(of pipe_name: String) -> String {
  case string.split_once(pipe_name, on: "/") {
    Ok(#(namespace, _)) -> namespace
    Error(Nil) -> pipe_name
  }
}

fn start_http_server(state: AppState, bind bind: String, on port: Int) {
  mist.new(make_handler(state))
    |> mist.port(port)
    |> mist.bind(bind)
    |> mist.start
}

/// This includes serving from /var/www/public/.well-known/acme-challenge
fn start_redirecting_to_https() {
  let handler = fn(req) {
    case request.path_segments(req) {
      [".well-known", "acme-challenge", challenge_file] -> {
        let path = "/var/www/public/.well-known/acme-challenge/" <> challenge_file
        logging.log(logging.Info, "Attempting to serve " <> challenge_file <> " at " <> path)
        case mist.send_file(path, offset: 0, limit: None) {
          Ok(file) -> response.new(200) |> response.set_body(file)
          Error(_) -> response.new(404) |> response.set_body(mist.Bytes(bytes_tree.from_string("not found")))
        }
      }

      _ -> {
        let u = request.to_uri(req)
        let https_uri = uri.Uri(
          scheme: Some("https"),
          userinfo: u.userinfo,
          host: u.host,
          port: None,
          path: u.path,
          query: u.query,
          fragment: u.fragment,
        )

        response.new(301)
        |> response.set_header("location", uri.to_string(https_uri))
        |> response.set_body(mist.Bytes(bytes_tree.from_string(
          "Try:\n\tcurl " <> colorize_uri(https_uri)
        )))
      }
    }
  }

  mist.new(handler)
    |> mist.port(80)
    |> mist.bind("0.0.0.0")
    |> mist.start
}

fn start_https_server(
  state: AppState,
  on interface: String,
  with config: tls.Config,
) {
  mist.new(make_handler(state))
    |> mist.port(443)
    |> mist.bind(interface)
    |> mist.with_tls(certfile: config.fullchain, keyfile: config.privkey)
    |> mist.start
}

@external(erlang, "erlang", "register")
pub fn register(name: Atom, pid: Pid) -> Bool

@external(erlang, "hot", "identity")
fn assume_pipemaster_message(dyn: dynamic.Dynamic) -> pipemaster.Message

@external(erlang, "hot", "identity")
fn assume_db_sync_message(dyn: dynamic.Dynamic) -> DbSyncMessage

pub fn forward_pipe_entries_forever(master: pipemaster.Pipemaster) {
  let selector = process.new_selector()
    |> process.select_other(assume_pipemaster_message)

  process.selector_receive_forever(selector) |> process.send(master.data, _)
  forward_pipe_entries_forever(master)
}

pub type SyncHint {
  LatestID(BitArray)
  NoID
  ErrorRetrievingID
}

pub type DbSyncMessage {
  /// Tells a node that a new entry has been inserted.
  /// Unfortunately right now all the node can do from here is call tell_nodes_where_were_at
  /// which results in a lot of back-and-forth.
  DbSyncNewEntry(who: Peer)

  DbSyncHint(
    who: Peer,
    latest_pipe_entry_id: SyncHint,
    latest_pipe_settings_id: SyncHint,
  )

  DbSyncExec(
    sql: String,
    with: List(pturso.Param),
  )
}

fn sync_hint_from_result(input: Result(dict.Dict(String, BitArray), Nil), key: String) -> SyncHint {
  case result.map(input, dict.get(_, key)) {
    Ok(Ok(value)) -> LatestID(value)
    Ok(Error(Nil)) -> NoID
    Error(Nil) -> ErrorRetrievingID
  }
}

fn latest_x_by_node(table: String, on db: pturso.Connection) {
  let expecting = {
    use node <- decode.field(0, decode.string)
    use latest_id <- decode.field(1, decode.bit_array)
    decode.success(#(node, latest_id))
  }
  let sql =
    "select node, max(id) as latest_id
    from "<>table<>"
    group by node"

  pturso.query(sql, on: db, with: [], expecting:)
  |> result.lazy_unwrap(list.new)
  |> dict.from_list
}

pub fn tell_nodes_where_were_at(db: pturso.Connection, peers: List(Peer), me: Peer) {
  let pipe_entries_subj = process.new_subject()
  process.spawn(fn() {
    latest_x_by_node("pipe_entries", on: db) |> process.send(pipe_entries_subj, _)
  })

  let pipe_settings_subj = process.new_subject()
  process.spawn(fn() {
    latest_x_by_node("pipe_settings", on: db) |> process.send(pipe_settings_subj, _)
  })

  let pipe_entries = process.receive(pipe_entries_subj, 1000)
  let pipe_settings = process.receive(pipe_settings_subj, 1000)

  let all = peers
    |> list.map(fn(peer) {
      let peer_string = peer_node(peer)
      #(
        peer_string,
        DbSyncHint(
          who: me,
          latest_pipe_entry_id: sync_hint_from_result(pipe_entries, peer_string),
          latest_pipe_settings_id: sync_hint_from_result(pipe_settings, peer_string),
        ),
      )
    })
    |> dict.from_list

  peers |> list.each(fn(peer) {
    let peer_string = peer_node(peer)
    dict.get(all, peer_string) |> result.map(fn(message) {
      peer_send_db_sync(peer, message)
    })
  })
}

fn expect(value: Result(a, b), or_return default: fn() -> r, when_ok continue: fn(a) -> r) {
  case value {
    Ok(x) -> continue(x)
    Error(_) -> default()
  }
}

fn process_sync_hint(
  hint: SyncHint,
  for table: String,
  with fields: List(#(String, decode.Decoder(pturso.Param))),
  on db: pturso.Connection,
  from me: Peer,
  to who: Peer,
) {
  let latest_id = case hint {
    LatestID(id) -> Ok(id)
    NoID -> gulid.from_parts(0, 0) |> gulid.to_bitarray |> Ok
    ErrorRetrievingID -> Error(Nil)
  }

  use latest_id <- expect(latest_id, or_return: fn() { Nil })

  let sql = string_tree.from_string("SELECT ")
    |> string_tree.append_tree(
      fields
      |> list.map(fn(x) { x.0 |> string_tree.from_string })
      |> string_tree.join(", ")
    )
    |> string_tree.append(" FROM ")
    |> string_tree.append(table)
    |> string_tree.append(" WHERE node = ? AND id > ? ORDER BY id ASC LIMIT 100")
    |> string_tree.to_string

  let with = [
    peer_node(me) |> pturso.String,
    latest_id |> pturso.Blob,
  ]

  let assert Ok(values) = pturso.query(sql, on: db, with:, expecting: decode.dynamic)

  case values {
    [_, ..] -> {
      let sql = string_tree.from_string("INSERT INTO ")
        |> string_tree.append(table)
        |> string_tree.append(" (")
        |> string_tree.append_tree(
          fields
          |> list.map(fn(x) { x.0 |> string_tree.from_string })
          |> string_tree.join(", ")
        )
        |> string_tree.append(") VALUES ")
        |> string_tree.append_tree(
          values
          |> list.map(fn(_) {
            string_tree.from_string("(")
            |> string_tree.append_tree(
              fields
              |> list.map(fn(_) { "?" |> string_tree.from_string })
              |> string_tree.join(", ")
            )
            |> string_tree.append(")")
          })
          |> string_tree.join(",")
        )
        |> string_tree.append(" ON CONFLICT DO NOTHING")
        |> string_tree.to_string

      let len = list.length(fields)
      let fields = fields
        |> list.zip(list.range(0, len - 1))
        |> list.map(fn(field) {
          let #(#(field_name, decoder), index) = field
          let extract = decode.field(index, decoder, decode.success)

          fn(value) {
            case decode.run(value, extract) {
              Ok(x) -> x
              Error(decode_errors) -> {
                logging.log(logging.Error, "Failed to decode " <> field_name <> ": " <> string.inspect(decode_errors))
                pturso.Null
              }
            }
          }
        })

      let params = list.flat_map(values, fn(value) {
        list.map(fields, fn(decode) { decode(value) })
      })

      let followup = DbSyncExec(sql:, with: params)
      logging.log(logging.Info, "Sending " <> int.to_string(list.length(values)) <> " " <> table)
      peer_send_db_sync(who, followup)
    }
    [] -> Nil
  }
}

pub fn db_receive_loop_iter(message: DbSyncMessage, db: pturso.Connection, me: Peer) {
  case message {
    DbSyncNewEntry(who:) -> {
      tell_nodes_where_were_at(db, [who], me)
    }

    DbSyncHint(who, latest_pipe_entry_id, latest_pipe_settings_id) -> {
      let subj = process.new_subject()

      process.spawn_unlinked(fn() {
        process_sync_hint(
          latest_pipe_entry_id,
          for: "pipe_entries",
          with: [
            #("id", decode.map(decode.bit_array, pturso.Blob)),
            #("node", decode.map(decode.string, pturso.String)),
            #("pipe", decode.map(decode.string, pturso.String)),
            #("method", decode.map(decode.string, pturso.String) |> pturso.nullable),
            #("headers", decode.map(decode.string, pturso.String) |> pturso.nullable),
            #("body", decode.map(decode.bit_array, pturso.Blob) |> pturso.nullable),
          ],
          on: db,
          from: me,
          to: who,
        )
        process.send(subj, Nil)
      })

      process.spawn_unlinked(fn() {
        process_sync_hint(
          latest_pipe_settings_id,
          for: "pipe_settings",
          with: [
            #("id", decode.map(decode.bit_array, pturso.Blob)),
            #("node", decode.map(decode.string, pturso.String)),
            #("namespace", decode.map(decode.string, pturso.String)),
            #("flags", decode.map(decode.int, pturso.Int)),
          ],
          on: db,
          from: me,
          to: who,
        )
        process.send(subj, Nil)
      })

      // Wait a little bit for each to finish
      let _ = process.receive(subj, within: 500)
      let _ = process.receive(subj, within: 500)

      Nil
    }

    DbSyncExec(sql, with) -> {
      logging.log(logging.Info, "Received SQL from peer")
      case pturso.query(sql, on: db, with:, expecting: decode.success(Nil)) {
        Ok(_) -> Nil
        Error(err) -> {
          logging.log(logging.Error, "Bad SQL over sync? " <> string.inspect(err) <> " -- " <> sql)
        }
      }
    }
  }
}

pub fn peer_receive_db_sync_forever(peer: Peer) {
  case peer {
    LocalPeer(db_sync_subj:, ..) -> process.receive_forever(db_sync_subj)

    RemotePeer(..) -> process.new_selector()
      |> process.select_other(assume_db_sync_message)
      |> process.selector_receive_forever
  }
}

pub fn peer_receive_db_sync(peer: Peer, within within: Int) {
  case peer {
    LocalPeer(db_sync_subj:, ..) -> process.receive(db_sync_subj, within:)

    RemotePeer(..) -> process.new_selector()
      |> process.select_other(assume_db_sync_message)
      |> process.selector_receive(within:)
  }
}

pub fn db_receive_loop(peer: Peer, db: pturso.Connection) {
  peer_receive_db_sync_forever(peer) |> db_receive_loop_iter(db, peer)

  hot_db_receive_loop(peer, db)
}

pub fn db_send_loop(db: pturso.Connection, peers: List(Peer), me: Peer) {
  tell_nodes_where_were_at(db, peers, me)
  process.sleep(300_000)
  hot_db_send_loop(db, peers, me)
}

pub fn main() {
  logging.configure()
  logging.set_level(logging.Info)

  let assert Ok(turso) = pturso.start()

  let db = pturso.connect(turso, "db", log_with: fn(entry) {
    logging.log(logging.Info, "SQL: " <> string.replace(entry.sql, each: "\n", with: " ") <> " [" <> int.to_string(entry.duration_ms) <> "ms]")
  })

  process.spawn(fn() {
    case migrations.migrate(db, migrations.all_migrations()) {
      Ok([]) -> logging.log(logging.Info, "No migrations to run")
      Ok(ran) -> logging.log(logging.Info, "Ran migrations: " <> string.join(ran, ", "))
      Error(#(ran, failed, error)) -> {
        case ran {
          [_, ..] as did_run -> logging.log(logging.Info, "Ran migrations: " <> string.join(did_run, ", "))
          _ -> Nil
        }
        logging.log(logging.Error, "Failed to run " <> failed <> ": " <> string.inspect(error))
      }
    }
  })

  let assert Ok(master) = pipemaster.new(counter.new_memory())

  let me = RemotePeer(
    address: my_erlang_node_id(),
    pipe_entry_name: pipe_entry_receiver_name(),
    db_sync_name: db_sync_receiver_name(),
  )

  let state = AppState(
    master: master.data,
    ulid_to_string: gulid.to_string_function(),
    ulid_from_string: gulid.from_string_function(),
    db:,
    self: me,
    peers: globally_configured_erlang_peers(),
  )

  let assert Ok(app_url) = case env.get("HOOK_URL") {
    Some(url) -> url
    None -> "http://localhost:8080"
  }
  |> uri.parse

  process.spawn(fn() {
    register(me.pipe_entry_name, process.self())
    forward_pipe_entries_forever(master)
  })

  process.spawn(fn() {
    register(me.db_sync_name, process.self())
    db_receive_loop(me, db)
  })

  process.spawn(fn() {
    db_send_loop(db, state.peers, me)
  })

  let _ = case app_url.scheme, app_url.host {
    Some("https"), Some(host) -> {
      let tls_config_path = "/etc/letsencrypt/renewal/" <> host <> ".conf"

      let https_start = tls.parse_config(at: tls_config_path)
      |> result.map(start_https_server(state, on: "::", with: _))

      case https_start {
        Ok(Ok(_)) -> Nil
        Ok(Error(failed_to_start)) -> {
          echo failed_to_start
          panic as "failed to start https server"
        }
        Error(config_failed) -> logging.log(logging.Error, "Bad tls config: " <> string.inspect(config_failed))
      }

      let assert Ok(_) = start_redirecting_to_https()
    }

    _, host -> {
      let assert Ok(_) = start_http_server(
        state,
        bind: option.unwrap(host,  "localhost"),
        on: option.unwrap(app_url.port, 8080),
      )
    }
  }

  process.sleep_forever()
}
