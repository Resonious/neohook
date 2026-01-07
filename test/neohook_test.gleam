import gleam/json
import gleam/dynamic/decode
import gleam/erlang/atom
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

pub fn curl_test() {
  let assert Ok(master) = pipemaster.new()
  let db = turso_connection()
  let state = neohook.AppState(
    master: master.data,
    ulid_to_string: gulid.to_string_function(),
    db:,
    peers: [],
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
    peers: [],
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

pub fn peer_test() {
  let assert Ok(master1) = pipemaster.new()
  let assert Ok(master2) = pipemaster.new()
  let db1 = turso_connection()
  let db2 = turso_connection()

  let peer1 = neohook.Peer(
    node: neohook.my_erlang_node_id(),
    pipe_entry_name: atom.create("pe1"),
    db_sync_name: atom.create("dbs1"),
  )

  let peer2 = neohook.Peer(
    node: neohook.my_erlang_node_id(),
    pipe_entry_name: atom.create("pe2"),
    db_sync_name: atom.create("dbs2"),
  )

  let state1 = neohook.AppState(
    master: master1.data,
    ulid_to_string: gulid.to_string_function(),
    db: db1,
    peers: [peer2],
  )

  let state2 = neohook.AppState(
    master: master2.data,
    ulid_to_string: gulid.to_string_function(),
    db: db2,
    peers: [peer1],
  )

  // neohook.register(peer1.db_sync_name, process.self())
  neohook.register(peer2.db_sync_name, process.self())

  // Send something with state1
  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_path("/test/1")
    |> request.set_header("user-agent", "curl/8.0.0")
    |> request.set_header("x-test-header", "HITHERE")
    |> request.set_body(http_wrapper.SimpleBody(bit_array.from_string("Sent to 1"), on_sse: no_sse))

  let response.Response(status:, headers: _, body: _) = neohook.http_handler(req, state1)
  should.equal(status, 200)

  // It should appear in its own DB
  let req = request.new()
    |> request.set_method(http.Get)
    |> request.set_path("/api/pipe_entries")
    |> request.set_query([#("pipe", "test/1")])
    |> request.set_body(http_wrapper.SimpleBody(bit_array.from_string(""), on_sse: no_sse))

  let response.Response(status:, headers: _, body:) = neohook.http_handler(req, state1)
  should.equal(status, 200)

  let assert mist.Bytes(body) = body
  let decoder = {
    use id <- decode.field("id", decode.string)
    use body <- decode.field("body", decode.bit_array)
    decode.success(#(id, body))
  } |> decode.list
  let assert Ok([#(id_in_state1, body)]) = json.parse_bits(bytes_tree.to_bit_array(body), decoder)

  should.equal(body, bit_array.from_string("Sent to 1"))

  // It should appear in state2's DB too
  let assert Ok(Nil) = neohook.db_receive_loop_iter(db2, wait_for: 3000)

  let req = request.new()
    |> request.set_method(http.Get)
    |> request.set_path("/api/pipe_entries")
    |> request.set_query([#("pipe", "test/1")])
    |> request.set_body(http_wrapper.SimpleBody(bit_array.from_string(""), on_sse: no_sse))

  let response.Response(status:, headers: _, body:) = neohook.http_handler(req, state2)
  should.equal(status, 200)

  let assert mist.Bytes(body) = body
  let decoder = {
    use id <- decode.field("id", decode.string)
    use body <- decode.field("body", decode.bit_array)
    decode.success(#(id, body))
  } |> decode.list
  let assert Ok([#(id_in_state2, body)]) = json.parse_bits(bytes_tree.to_bit_array(body), decoder)

  should.equal(body, bit_array.from_string("Sent to 1"))
  should.equal(id_in_state2, id_in_state1)
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

@external(erlang, "fs", "delete_files_matching")
fn delete_files_matching(pattern: String) -> Nil
