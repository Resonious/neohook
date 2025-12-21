import gleam/string
import gleam/string_tree
import pipemaster
import pipe
import gleam/http
import gleam/result
import gleam/erlang/process
import gleam/http/request
import logging
import gleam/http/response

import ewe.{type Request, type Response}

fn listen_on_pipe(
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
    handler: fn(body, pipe_id, message) {
      // TODO: I guess we would switch on user-agent here..?
      case pipe.handle(pipe.Curl(body), message) {
        pipe.Dead -> {
          process.send(master, pipemaster.CleanPipe(pipe_name, pipe_id))
          ewe.chunked_stop()
        }
        _ -> ewe.chunked_continue(pipe_id)
      }
    },
    on_close: fn(_conn, _state) {
      logging.log(logging.Info, "Bye bye??")
    }
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

fn http_handler(req: Request, master: pipemaster.Subject) -> Response {
  logging.log(logging.Info, "what the??")

  let user_agent = request.get_header(req, "user-agent")
    |> result.unwrap("unknown")

  case request.path_segments(req) {
    [] -> 
      response.new(200)
      |> response.set_header("content-type", "text/plain; charset=utf-8")
      |> response.set_body(ewe.TextData("Hello, World!"))

    parts -> {
      let pipe_name = compute_pipe_name(parts)

      case req.method {
        http.Get -> listen_on_pipe(req, pipe_name, master)
        http.Post -> send_to_pipe(req, pipe_name, master)
        _ -> 
          response.new(405)
          |> response.set_header("content-type", "text/plain; charset=utf-8")
          |> response.set_body(ewe.TextData("method not allowed"))
      }
    }
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
