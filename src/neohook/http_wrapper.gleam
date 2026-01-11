import gleam/option.{type Option, None, Some}
import gleam/string_tree
import gleam/function
import gleam/result
import gleam/bytes_tree
import gleam/otp/actor
import gleam/http/request
import gleam/http/response
import gleam/erlang/process.{type Subject}
import mist

pub type Body {
  MistBody(mist.Connection)
  SimpleBody(bytes: BitArray, on_sse: fn(SSEEvent) -> Result(Nil, Nil))
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

pub type SSEEvent {
  SSEEvent(name: Option(String), data: string_tree.StringTree)
}

pub type SSEConnection {
  MistSSEConnection(mist.SSEConnection)
  FakeSSEConnection(callback: fn(SSEEvent) -> Result(Nil, Nil))
}

pub fn ip_address(of req: Request) -> Result(String, Nil) {
  case req.body {
    MistBody(conn) -> mist.get_connection_info(conn)
      |> result.map(fn(x) { x.ip_address })
      |> result.map(mist.ip_address_to_string)
    SimpleBody(..) -> Ok("127.0.0.1")
  }
}

pub fn send_sse_event(
  data: string_tree.StringTree,
  to conn: SSEConnection,
) -> Result(Nil, Nil) {
  case conn {
    MistSSEConnection(conn) -> mist.event(data) |> mist.send_event(conn, _)
    FakeSSEConnection(callback:) -> callback(SSEEvent(name: None, data:))
  }
}

pub fn send_sse_named_event(
  data: string_tree.StringTree,
  name name: String,
  to conn: SSEConnection,
) -> Result(Nil, Nil) {
  case conn {
    MistSSEConnection(conn) -> mist.event(data) |> mist.event_name(name) |> mist.send_event(conn, _)
    FakeSSEConnection(callback:) -> callback(SSEEvent(name: Some(name), data:))
  }
}

pub fn server_sent_events(
  request req: Request,
  initial_response resp: response.Response(discard),
  init init: fn(Subject(message)) ->
    Result(actor.Initialised(state, message, data), String),
  loop loop: fn(state, message, SSEConnection) -> actor.Next(state, message),
) -> Response {
  case req {
    request.Request(body: MistBody(conn), ..) -> mist.server_sent_events(
      convert_request(req, conn),
      resp,
      init,
      loop: fn(state, message, conn) { loop(state, message, MistSSEConnection(conn)) }
    )

    request.Request(body: SimpleBody(on_sse:, ..), ..) -> {
      actor.new_with_initialiser(1000, fn(subj) {
        init(subj)
        |> result.map(fn(return) { actor.returning(return, subj) })
      })
      |> actor.on_message(fn(state, message) {
        loop(state, message, FakeSSEConnection(callback: on_sse))
      })
      |> actor.start
      |> result.map(fn(subj) {
        let assert Ok(sse_process) = process.subject_owner(subj.data)
        let monitor = process.monitor(sse_process)
        let selector =
          process.new_selector()
          |> process.select_specific_monitor(monitor, function.identity)
        response.new(200)
        |> response.set_body(mist.ServerSentEvents(selector))
      })
      |> result.lazy_unwrap(fn() {
        response.new(400) |> response.set_body(mist.Bytes(bytes_tree.new()))
      })
    }
  }
}

pub fn read_body(
  req: Request,
  max_body_limit max_body_limit: Int,
) -> Result(request.Request(BitArray), mist.ReadError) {
  case req {
    request.Request(body: MistBody(conn), ..) -> mist.read_body(convert_request(req, conn), max_body_limit:)
    request.Request(body: SimpleBody(buffer, ..), ..) -> Ok(convert_request(req, buffer))
  }
}
