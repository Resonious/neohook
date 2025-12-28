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
import gleam/erlang/process
import gleam/http/request
import logging
import gleam/http/response
import gleam/bytes_tree
import gleam/otp/actor
import mist

@external(erlang, "hot", "make_handler")
fn make_handler(master: pipemaster.Subject) -> fn(Request) -> Response

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
  case mist.read_body(req, 1024 * 100 * 50) {
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

fn serve_html(path: String) -> Response {
  let assert Ok(file) = mist.send_file(path, offset: 0, limit: None)

  response.new(200)
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

pub fn http_handler(req: Request, master: pipemaster.Subject) -> Response {
  use <- log_request(req)

  // It seems the "//" path results in an empty string path
  use <- lazy_guard(when: req.path == "", return: redirect(to: "/"))

  // Remove repeated slashes from paths
  let scrubbed_path = remove_repeat_slashes(from: req.path)
  use <- lazy_guard(when: req.path != scrubbed_path, return: redirect(to: scrubbed_path))

  // routing
  case request.path_segments(req) {
    [] -> serve_html("static/landing.html")
    ["favicon.png"] -> serve_static("static/favicon.png", "image/png")
    ["favicon.svg"] -> serve_static("static/favicon.svg", "image/svg+xml")

    // TODO: do not allow "." in any of these parts..!
    parts -> {
      let pipe_name = compute_pipe_name(parts)

      case req.method, infer_requester_type(from_headers: req.headers) {
        http.Get, CurlRequester -> listen_on_pipe_for_curl(req, pipe_name, master)
        http.Get, SseRequester -> listen_on_pipe_for_sse(req, pipe_name, master)
        http.Get, UnknownRequester -> serve_html("static/view.html")
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
  mist.new(make_handler(master.data))
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
  master: pipemaster.Pipemaster,
  on interface: String,
  with config: tls.Config,
) {
  mist.new(make_handler(master.data))
    |> mist.port(443)
    |> mist.bind(interface)
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

      let https_start = tls.parse_config(at: tls_config_path)
      |> result.map(start_https_server(master, on: "::", with: _))

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
        master,
        bind: option.unwrap(host,  "localhost"),
        on: option.unwrap(app_url.port, 8080),
      )
    }
  }

  process.sleep_forever()
}
