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
import gleam/yielder
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
fn hot_db_receive_loop(db: pturso.Connection) -> Nil

@external(erlang, "hot", "db_send_loop")
fn hot_db_send_loop(db: pturso.Connection) -> Nil

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

fn listen_on_pipe_for_curl(
  req: Request,
  pipe_name: String,
  master: pipemaster.Subject,
) -> Response {
  let receiver = process.new_subject()
  let pid = process.self()

  let colorized_url = req |> request.to_uri |> colorize_uri

  let iter = yielder.once(fn() {
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
  })
  |> yielder.append(
    yielder.repeatedly(fn() {
      process.receive_forever(receiver)
    })
  )

  let id = pipemaster.new_pipe_id()
  case pipe.new(pipe.Curl(receiver, pid)) {
    Ok(actor) -> {
      process.send(master, pipemaster.AddPipe(
        pipe_name,
        id,
        actor.data,
      ))

      response.new(200)
      |> response.set_body(mist.Chunked(iter))
      |> response.set_header("content-type", "application/octet-stream")
    }

    Error(err) -> {
      echo err
      response.new(500)
      |> response.set_body(mist.Bytes(bytes_tree.from_string("uh oh")))
    }
  }
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
      let selector = process.new_selector()
        |> process.select(subject)
      actor.initialised(SseState(id, pipe_name, master))
      |> actor.selecting(selector)
      |> Ok
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

fn globally_configured_erlang_peers() -> List(Atom) {
  env.get("ERLANG_PEERS")
  |> option.map(string.split(_, ","))
  |> option.lazy_unwrap(fn() { [] })
  |> list.map(atom.create)
}

fn my_erlang_node_id() -> Atom {
  let ip = env.get("ERLANG_NODE_IP") |> option.unwrap("localhost")
  atom.create("neohook@" <> ip)
}

@external(erlang, "erlang", "send")
fn send_to_remote(destination: #(Atom, Atom), message: a) -> a

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

fn send_to_pipe(req: Request, pipe_name: String, state: AppState) -> Response {
  case http_wrapper.read_body(req, 1024 * 100 * 50) {
    Ok(req) -> {
      let id = gulid.new()
      let message = pipemaster.PushEntry(
        pipe_name,
        pipe.Entry(
          id: gulid.to_bitarray(id),
          method: req.method,
          headers: [#("x-snd-id", state.ulid_to_string(id)), ..req.headers],
          body: req.body,
        )
      )

      process.send(state.master, message)

      let headers_json = message.entry.headers
      |> list.map(fn(x) { #(x.0, json.string(x.1)) })
      |> json.object
      |> json.to_string

      let node = my_erlang_node_id() |> atom.to_string

      let #(sql, with) = sql.insert_pipe_entry(
        id: message.entry.id,
        node:,
        pipe: pipe_name,
        method: Some(message.entry.method |> http.method_to_string),
        headers: Some(headers_json),
        body: Some(message.entry.body),
      )
      let with = with |> list.map(parrot_to_pturso)
      case pturso.query(sql, on: state.db, with:, expecting: decode.success(Nil)) {
        Error(error) -> { echo error Nil }
        _ -> Nil
      }

      // Update peers (both database and realtime)
      let #(sql, params) = pipe_entry_bulk_insert([sql.PipeEntriesFromNodeSince(
        id: message.entry.id,
        node:,
        pipe: pipe_name,
        method: Some(message.entry.method |> http.method_to_string),
        headers: Some(headers_json),
        body: Some(message.entry.body),
      )])
      let sync_command = DbSyncExec(sql:, with: params)

      globally_configured_erlang_peers()
      |> list.each(fn(peer) {
        logging.log(logging.Info, "sending to " <> atom.to_string(peer))
        send_to_remote(#(pipe_entry_receiver_name(), peer), message)
        send_to_remote(#(db_sync_receiver_name(), peer), sync_command)
      })

      response.new(200)
      |> response.set_header("content-type", "text/plain")
      |> response.set_body(mist.Bytes(bytes_tree.from_string("Sent\n")))
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
    db: pturso.Connection,
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

    ["api", "pipe_entries"] -> {
      let pipe = req.query
        |> option.unwrap("")
        |> uri.parse_query
        |> result.unwrap([])
        |> list.key_find("pipe")

      use <- lazy_guard(when: result.is_error(pipe), return: bad_request(because: "missing `?pipe=...`"))
      let assert Ok(pipe) = pipe

      let #(sql, with, expecting) = sql.pipe_entries_by_pipe(pipe:, limit: 10, offset: 0)
      let with = with |> list.map(parrot_to_pturso)

      let render_body = fn(body: BitArray) -> String {
        case bit_array.to_string(body) {
          Ok(x) -> x
          Error(_) -> bit_array.base64_encode(body, True)
        }
      }

      let entries_json = pturso.query(sql, on: state.db, with:, expecting:)
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
        |> json.to_string_tree

      response.new(200)
      |> response.set_header("content-type", "application/json")
      |> response.set_body(mist.Bytes(bytes_tree.from_string_tree(entries_json)))
    }

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
        |> response.set_body(mist.Bytes(bytes_tree.from_string("redirecting to https")))
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
fn register(name: Atom, pid: Pid) -> Bool

@external(erlang, "hot", "identity")
fn assume_pipemaster_message(dyn: dynamic.Dynamic) -> pipemaster.Message

@external(erlang, "hot", "identity")
fn assume_db_sync_message(dyn: dynamic.Dynamic) -> DbSyncMessage

fn forward_pipe_entries_forever(master: pipemaster.Pipemaster) {
  let selector = process.new_selector()
    |> process.select_other(assume_pipemaster_message)

  process.selector_receive_forever(selector) |> process.send(master.data, _)
  forward_pipe_entries_forever(master)
}

type DbSyncMessage {
  DbSyncHint(
    who: Atom,
    latest_pipe_entry_id: Result(BitArray, Nil),
  )

  DbSyncExec(
    sql: String,
    with: List(pturso.Param),
  )
}

fn tell_nodes_where_were_at(db: pturso.Connection) {
  let #(sql, with, expecting) = sql.latest_pipe_entries()
  let with = with |> list.map(parrot_to_pturso)

  // Parrot/sqlc doesn't understand max(id)
  let hack = fn(x) {
    let fake_id = fn() { gulid.new() |> gulid.to_bitarray }
    option.lazy_unwrap(x, fn() { fake_id() |> dynamic.bit_array })
    |> decode.run(decode.bit_array)
    |> result.lazy_unwrap(fake_id)
  }

  let latest = pturso.query(sql, on: db, with:, expecting:)
    |> result.lazy_unwrap(list.new)
    |> list.map(fn(x) { #(x.node, hack(x.latest_id)) })
    |> dict.from_list

  let peers = globally_configured_erlang_peers()

  let all = peers
    |> list.map(fn(peer) {
      let peer_string = atom.to_string(peer)
      #(
        peer |> atom.to_string,
        DbSyncHint(
          who: my_erlang_node_id(),
          latest_pipe_entry_id: dict.get(latest, peer_string),
        ),
      )
    })
    |> dict.from_list

  peers |> list.each(fn(peer) {
    let peer_string = atom.to_string(peer)
    dict.get(all, peer_string) |> result.map(fn(message) {
      send_to_remote(#(db_sync_receiver_name(), peer), message)
    })
  })
}

fn pipe_entry_bulk_insert(entries: List(sql.PipeEntriesFromNodeSince)) -> #(String, List(pturso.Param)) {
  let sql = string_tree.new()
    |> string_tree.append("INSERT INTO pipe_entries (id, node, pipe, method, headers, body) VALUES ")
    |> string_tree.append_tree(
        entries
        |> list.map(fn(_) { string_tree.from_string("(?, ?, ?, ?, ?, ?)") })
        |> string_tree.join(","))
    |> string_tree.append(" ON CONFLICT DO NOTHING")
    |> string_tree.to_string

    let params = entries
      |> list.flat_map(fn(e) { [
        pturso.Blob(e.id),
        pturso.String(e.node),
        pturso.String(e.pipe),
        option.map(e.method, pturso.String) |> option.unwrap(pturso.Null),
        option.map(e.headers, pturso.String) |> option.unwrap(pturso.Null),
        option.map(e.body, pturso.Blob) |> option.unwrap(pturso.Null),
      ] })

  #(sql, params)
}

pub fn db_receive_loop(db: pturso.Connection) {
  let selector = process.new_selector()
    |> process.select_other(assume_db_sync_message)

  case process.selector_receive_forever(from: selector) {
    DbSyncHint(who, latest_pipe_entry_id) -> {
      let me = my_erlang_node_id() |> atom.to_string

      let latest_pipe_entry_id = case latest_pipe_entry_id {
        Ok(p) -> p
        Error(Nil) -> gulid.from_parts(0, 0) |> gulid.to_bitarray
      }

      let #(sql, with, expecting) = sql.pipe_entries_from_node_since(
        node: me, id: latest_pipe_entry_id,
      )
      let with = with |> list.map(parrot_to_pturso)

      let entries = pturso.query(sql, on: db, with:, expecting:) |> result.lazy_unwrap(list.new)

      case entries {
        [_, ..] -> {
          let #(sql, params) = pipe_entry_bulk_insert(entries)
          let followup = DbSyncExec(sql:, with: params)
          logging.log(logging.Info, "Sending " <> int.to_string(list.length(entries)) <> " pipe entries")
          send_to_remote(#(db_sync_receiver_name(), who), followup)
          Nil
        }

        _ -> Nil
      }
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

  hot_db_receive_loop(db)
}

pub fn db_send_loop(db: pturso.Connection) {
  tell_nodes_where_were_at(db)
  process.sleep(300_000)
  hot_db_send_loop(db)
}

pub fn main() {
  logging.configure()
  logging.set_level(logging.Info)

  // TODO: maybe the library should get the binary for you..?
  let erso_path = env.get("ERSO") |> option.unwrap("/var/www/erso")
  let assert Ok(turso) = pturso.start(erso_path)

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

  let assert Ok(master) = pipemaster.new()

  let state = AppState(
    master: master.data,
    ulid_to_string: gulid.to_string_function(),
    db:,
  )

  let assert Ok(app_url) = case env.get("HOOK_URL") {
    Some(url) -> url
    None -> "http://localhost:8080"
  }
  |> uri.parse

  process.spawn(fn() {
    register(pipe_entry_receiver_name(), process.self())
    forward_pipe_entries_forever(master)
  })

  process.spawn(fn() {
    register(db_sync_receiver_name(), process.self())
    db_receive_loop(db)
  })

  process.spawn(fn() {
    db_send_loop(db)
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
