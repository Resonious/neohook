import gleam/bytes_tree
import gleam/otp/actor
import gleam/http/request
import gleam/http/response
import gleam/erlang/process.{type Subject}
import mist

pub type Body {
  MistBody(mist.Connection)
  SimpleBody(BitArray)
}
pub type Request = request.Request(Body)
pub type Response = response.Response(mist.ResponseData)

pub fn convert_request(req: request.Request(a), body: b) -> request.Request(b) {
  request.Request(
    method: req.method,
    headers: req.headers,
    body:,
    scheme: req.scheme,
    host: req.host,
    port: req.port,
    path: req.path,
    query: req.query,
  )
}

pub fn server_sent_events(
  request req: Request,
  initial_response resp: response.Response(discard),
  init init: fn(Subject(message)) ->
    Result(actor.Initialised(state, message, data), String),
  loop loop: fn(state, message, mist.SSEConnection) -> actor.Next(state, message),
) -> Response {
  case req {
    request.Request(body: MistBody(conn), ..) -> mist.server_sent_events(convert_request(req, conn), resp, init, loop)
    request.Request(body: SimpleBody(_), ..) -> response.new(421)
      |> response.set_body(mist.Bytes(bytes_tree.from_string("TODO")))
  }
}

pub fn read_body(
  req: Request,
  max_body_limit max_body_limit: Int,
) -> Result(request.Request(BitArray), mist.ReadError) {
  case req {
    request.Request(body: MistBody(conn), ..) -> mist.read_body(convert_request(req, conn), max_body_limit:)
    request.Request(body: SimpleBody(buffer), ..) -> Ok(convert_request(req, buffer))
  }
}
