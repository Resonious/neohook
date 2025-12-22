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

import ewe.{type Request, type Response}

fn listen_on_pipe_for_curl(
  req: Request,
  pipe_name: String,
  master: pipemaster.Subject,
) {
  ewe.chunked_body(
    req,
    response.new(200) |> response.set_header("content-type", "application/octet-stream"),
    on_init: fn(subject) {
      let id = pipemaster.new_pipe_id()
      process.send(master, pipemaster.AddPipe(
        name: pipe_name,
        id:,
        subject:,
      ))
      id
    },
    handler: fn(body, id, message) {
      case pipe.handle(pipe.Curl(body), message) {
        pipe.Dead -> {
          process.send(master, pipemaster.CleanPipe(pipe_name, remove_id: id))
          ewe.chunked_stop()
        }
        _ -> ewe.chunked_continue(id)
      }
    },
    on_close: fn(_conn, id) {
      process.send(master, pipemaster.CleanPipe(pipe_name, remove_id: id))
    }
  )
}

fn listen_on_pipe_for_sse(
  req: Request,
  pipe_name: String,
  master: pipemaster.Subject,
) {
  ewe.sse(
    req,
    on_init: fn(subject) {
      let id = pipemaster.new_pipe_id()
      process.send(master, pipemaster.AddPipe(
        name: pipe_name,
        id:,
        subject:,
      ))
      id
    },
    handler: fn(conn, id, message) {
      case pipe.handle(pipe.Sse(conn), message) {
        pipe.Dead -> {
          process.send(master, pipemaster.CleanPipe(pipe_name, remove_id: id))
          ewe.sse_stop()
        }
        _ -> ewe.sse_continue(id)
      }
    },
    on_close: fn(_conn, id) {
      process.send(master, pipemaster.CleanPipe(pipe_name, remove_id: id))
    },
  )
}

fn send_to_pipe(req: Request, pipe_name: String, master: pipemaster.Subject) {
  case ewe.read_body(req, bytes_limit: 1024 * 8) {
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
      |> response.set_body(ewe.TextData("Sent"))
    }
    Error(ewe.BodyTooLarge) ->
      response.new(413)
      |> response.set_header("content-type", "text/plain; charset=utf-8")
      |> response.set_body(ewe.TextData("Body too large"))
    Error(ewe.InvalidBody) ->
      response.new(400)
      |> response.set_header("content-type", "text/plain; charset=utf-8")
      |> response.set_body(ewe.TextData("Invalid request"))
  }
}

fn serve_browser() {
  let assert Ok(file) = ewe.file("static/view.html", offset: None, limit: None)

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
          |> response.set_body(ewe.TextData("method not allowed"))
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

  ewe.new(http_handler(_, master.data))
    |> ewe.bind_all()
    |> ewe.listening(port: 8080)
    |> ewe.start()
}

pub fn main() {
  logging.configure()
  logging.set_level(logging.Info)

  let assert Ok(_) = start_server()

  process.sleep_forever()
}
