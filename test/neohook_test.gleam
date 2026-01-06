import migrations
import gleam/string_tree
import gleam/option.{Some}
import gleam/erlang/process
import gleam/string
import gleeunit/should
import gleam/bytes_tree
import gleam/yielder
import mist
import gleam/http/response
import gleam/bit_array
import neohook/http_wrapper
import gleam/http/request
import gleam/http
import neohook
import gulid
import pipemaster
import gleam/int
import env
import pturso
import gleeunit

pub fn main() -> Nil {
  delete_files_matching("test/db*")
  gleeunit.main()
}

@external(erlang, "fs", "delete_files_matching")
fn delete_files_matching(pattern: String) -> Nil

pub fn curl_test() {
  let assert Ok(master) = pipemaster.new()
  let db = turso_connection()
  let state = neohook.AppState(
    master: master.data,
    ulid_to_string: gulid.to_string_function(),
    db:,
  )

  // Start listening
  let req = request.new()
    |> request.set_method(http.Get)
    |> request.set_path("/test/1")
    |> request.set_header("user-agent", "curl/8.0.0")
    |> request.set_body(http_wrapper.SimpleBody(bit_array.from_string(""), on_sse: no_sse))

  let response.Response(status:, headers: _, body:) = neohook.http_handler(req, state)
  should.equal(status, 200)
  // assert list.contains(headers, #("content-type", "application/octet-stream"))

  let assert mist.Chunked(listener) = body

  // Send something
  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_path("/test/1")
    |> request.set_header("user-agent", "curl/8.0.0")
    |> request.set_header("x-test-header", "HITHERE")
    |> request.set_body(http_wrapper.SimpleBody(bit_array.from_string("Message here"), on_sse: no_sse))

  let response.Response(status:, headers: _, body: _) = neohook.http_handler(req, state)
  should.equal(status, 200)

  // The message we sent should be readable from the listener
  let assert [welcome, message] = listener |> yielder.take(2) |> yielder.to_list
  let welcome = bytes_tree.to_bit_array(welcome)
  let assert Ok(message) = message |> bytes_tree.to_bit_array |> bit_array.to_string()

  assert bit_array.starts_with(welcome, bit_array.from_string("You are listening on"))
  assert string.contains(message, "x-test-header")
  assert string.contains(message, "HITHERE")
  assert string.contains(message, "Message here")
}

pub fn sse_test() {
  let assert Ok(master) = pipemaster.new()
  let db = turso_connection()
  let state = neohook.AppState(
    master: master.data,
    ulid_to_string: gulid.to_string_function(),
    db:,
  )

  let subj = process.new_subject()

  // Start listening
  let req = request.new()
    |> request.set_method(http.Get)
    |> request.set_path("/test/1")
    |> request.set_header("accept", "text/event-stream")
    |> request.set_body(http_wrapper.SimpleBody(
      bit_array.from_string(""),
      on_sse: fn(x) { process.send(subj, x) Ok(Nil) }
    ))

  let response.Response(status:, headers: _, body:) = neohook.http_handler(req, state)
  should.equal(status, 200)

  let assert mist.ServerSentEvents(_selector) = body

  // Send something
  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_path("/test/1")
    |> request.set_header("user-agent", "curl/8.0.0")
    |> request.set_header("x-test-header", "HITHERE")
    |> request.set_body(http_wrapper.SimpleBody(bit_array.from_string("Message here"), on_sse: no_sse))

  let response.Response(status:, headers: _, body: _) = neohook.http_handler(req, state)
  should.equal(status, 200)

  // The message we sent should have come in through the SSE connection
  let assert Ok(headers_event) = process.receive(from: subj, within: 2000)
  let assert Ok(body_event) = process.receive(from: subj, within: 2000)

  let assert Some("headers") = headers_event.name
  let received_headers = headers_event.data |> string_tree.to_string
  assert string.contains(does: received_headers, contain: "\"x-test-header\":\"HITHERE\"")

  should.be_none(body_event.name)
  let received_body = body_event.data |> string_tree.to_string
  assert string.contains(does: received_body, contain: "Message here")
}

///////////////////////
// Utility functions //
///////////////////////

fn turso_connection() -> pturso.Connection {
  let erso_path = env.get("ERSO") |> option.unwrap("/var/www/erso")
  let assert Ok(turso) = pturso.start(erso_path)
  let db_id = int.random(999999) |> int.to_string
  let conn = pturso.connect(turso, "test/db" <> db_id, log_with: fn(_) { Nil })
  let assert Ok(_) = migrations.migrate(conn, migrations.all_migrations())
  conn
}

fn no_sse(_event: http_wrapper.SSEEvent) -> Result(Nil, Nil) {
  Error(Nil)
}
