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
import gleam/erlang/process
import gleam/http/request
import logging
import gleam/http/response
import gleam/bytes_tree
import gleam/otp/actor
import mist

type Request = request.Request(mist.Connection)
type Response = response.Response(mist.ResponseData)

type SseState {
  SseState(
    id: Int,
    pipe_name: String,
    master: pipemaster.Subject,
  )
}

/// Same as uri.to_string but doesn't include the port when it's the
/// default for the protocol (80, 443).
pub fn better_uri_to_string(uri: uri.Uri) -> String {
  let parts = case uri.fragment {
    Some(fragment) -> ["#", fragment]
    None -> []
  }
  let parts = case uri.query {
    Some(query) -> ["?", query, ..parts]
    None -> parts
  }
  let parts = [uri.path, ..parts]
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

  let url = req |> request.to_uri |> better_uri_to_string

  let iter = yielder.once(fn() {
    bytes_tree.new()
    |> bytes_tree.append_string("You are listening on ")
    |> bytes_tree.append_string(termcolor.red)
    |> bytes_tree.append_string("/")
    |> bytes_tree.append_string(pipe_name)
    |> bytes_tree.append_string(termcolor.reset)
    |> bytes_tree.append_string("\n\n")
    |> bytes_tree.append_string("Try this in another terminal: ")
    |> bytes_tree.append_string(termcolor.cyan)
    |> bytes_tree.append_string("\n  curl -d 'Hello, World!' ")
    |> bytes_tree.append_string(url)
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
  mist.server_sent_events(
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

fn send_to_pipe(req: Request, pipe_name: String, master: pipemaster.Subject) -> Response {
  case mist.read_body(req, 1024 * 8) {
    Ok(req) -> {
      process.send(master, pipemaster.PushEntry(
        pipe_name,
        pipe.Entry(
          method: req.method,
          headers: req.headers,
          body: req.body,
        )
      ))

      response.new(200)
      |> response.set_header("content-type", "text/plain")
      |> response.set_body(mist.Bytes(bytes_tree.from_string("Sent")))
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

fn serve_browser() -> Response {
  let assert Ok(file) = mist.send_file("static/view.html", offset: 0, limit: None)

  response.new(200)
  |> response.set_header("content-type", "text/html; charset=utf-8")
  |> response.set_body(file)
}

fn http_handler(req: Request, master: pipemaster.Subject) -> Response {
  let method_str = http.method_to_string(req.method)
  let user_agent = request.get_header(req, "user-agent")
    |> result.unwrap("unknown")

  logging.log(logging.Info, method_str <> " " <> req.path <> " (" <> user_agent <> ")")

  case request.path_segments(req) {
    [] -> serve_browser()

    parts -> {
      let pipe_name = compute_pipe_name(parts)

      case req.method, infer_requester_type(from_headers: req.headers) {
        http.Get, CurlRequester -> listen_on_pipe_for_curl(req, pipe_name, master)
        http.Get, SseRequester -> listen_on_pipe_for_sse(req, pipe_name, master)
        http.Get, UnknownRequester -> serve_browser()
        http.Post, _ -> send_to_pipe(req, pipe_name, master)
        _, _ ->
          response.new(405)
          |> response.set_header("content-type", "text/plain; charset=utf-8")
          |> response.set_body(mist.Bytes(bytes_tree.from_string("method not allowed")))
      }
    }
  }
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

fn compute_pipe_name(parts: List(String)) -> String {
  string.join(parts, "/")
}

fn start_http_server(master: pipemaster.Pipemaster, bind bind: String, on port: Int) {
  mist.new(http_handler(_, master.data))
    |> mist.port(port)
    |> mist.bind(bind)
    |> mist.start
}

fn start_redirecting_to_https() {
  let handler = fn(req) {
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

  mist.new(handler)
    |> mist.port(80)
    |> mist.bind("0.0.0.0")
    |> mist.start
}

fn start_https_server(master: pipemaster.Pipemaster, with config: tls.Config) {
  mist.new(http_handler(_, master.data))
    |> mist.port(443)
    |> mist.bind("0.0.0.0")
    |> mist.with_tls(certfile: config.fullchain, keyfile: config.privkey)
    |> mist.start
}

pub fn main() {
  logging.configure()
  logging.set_level(logging.Info)
  let assert Ok(master) = pipemaster.new()

  let assert Ok(app_url) = case env.get("HOOK_URL") {
    Some(url) -> url
    None -> "http://localhost:8080"
  }
  |> uri.parse

  let _ = case app_url.scheme, app_url.host {
    Some("https"), Some(host) -> {
      let tls_config_path = "/etc/letsencrypt/renewal/" <> host <> ".conf"

      let assert Ok(_) = start_https_server(
        master,
        with: tls.parse_config(at: tls_config_path),
      )

      let assert Ok(_) = start_redirecting_to_https()
    }

    _, host -> {
      let assert Ok(_) = start_http_server(
        master,
        bind: option.unwrap(host,  "localhost"),
        on: option.unwrap(app_url.port, 8080),
      )
    }
  }

  process.sleep_forever()
}
