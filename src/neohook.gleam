import gleam/yielder
import gleam/dict
import gleam/string
import pipemaster
import pipe
import gleam/http
import gleam/result
import gleam/option.{None}
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

fn listen_on_pipe_for_curl(
  _req: Request,
  pipe_name: String,
  master: pipemaster.Subject,
) -> Response {
  let receiver = process.new_subject()
  let iter = yielder.repeatedly(fn() {
    process.receive_forever(receiver)
  })

  let id = pipemaster.new_pipe_id()
  case pipe.new(pipe.Curl(receiver)) {
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

fn start_server() {
  let assert Ok(master) = pipemaster.new()

  mist.new(http_handler(_, master.data))
    |> mist.port(8080)
    |> mist.start
}

pub fn main() {
  logging.configure()
  logging.set_level(logging.Info)

  let assert Ok(_) = start_server()

  process.sleep_forever()
}
