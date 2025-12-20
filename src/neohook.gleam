import gleam/uri
import gleam/erlang/process
import logging
import gleam/http/response

import ewe.{type Request, type Response}

fn handler(req: Request) -> Response {
  logging.log(logging.Info, "what the??")

  case uri.path_segments(req.path) {
    [] -> 
      response.new(200)
      |> response.set_header("content-type", "text/plain; charset=utf-8")
      |> response.set_body(ewe.TextData("Hello, World!"))

    [first, ..] -> 
      response.new(200)
      |> response.set_header("content-type", "text/plain; charset=utf-8")
      |> response.set_body(ewe.TextData("hello " <> first))
  }
}

fn start_server() {
  ewe.new(handler)
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
