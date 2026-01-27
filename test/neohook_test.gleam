import shcribe
import neohook/counter
import gleam/function
import gleam/list
import gleam/result
import gleam/json
import gleam/dynamic/decode
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
import pturso
import gleeunit

pub fn main() -> Nil {
  delete_files_matching("test/db*")
  gleeunit.main()
}

pub fn curl_test() {
  let ctr = counter.new_memory()
  let assert Ok(master) = pipemaster.new(ctr)
  let db = turso_connection()
  let state = neohook.AppState(
    master: master.data,
    ulid_to_string: gulid.to_string_function(),
    ulid_from_string: gulid.from_string_function(),
    db:,
    self: default_peer(),
    peers: [],
  )

  // Collect chunks via callback
  let chunk_subj = process.new_subject()

  // Start listening
  let req = request.new()
    |> request.set_method(http.Get)
    |> request.set_path("/test/1")
    |> request.set_header("user-agent", "curl/8.0.0")
    |> request.set_body(http_wrapper.SimpleBody(
      bit_array.from_string(""),
      on_sse: no_sse,
      on_chunk: fn(data) { process.send(chunk_subj, data) Ok(Nil) },
    ))

  let response.Response(status:, headers: _, body:) = neohook.http_handler(req, state)
  should.equal(status, 200)

  let assert mist.Chunked = body

  // Send something
  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_path("/test/1")
    |> request.set_header("user-agent", "curl/8.0.0")
    |> request.set_header("x-test-header", "HITHERE")
    |> request.set_body(http_wrapper.SimpleBody(
      bit_array.from_string("Message here"),
      on_sse: no_sse,
      on_chunk: no_chunk,
    ))

  let response.Response(status:, headers: _, body: _) = neohook.http_handler(req, state)
  should.equal(status, 200)

  // The welcome message should have come through
  let assert Ok(welcome) = process.receive(from: chunk_subj, within: 2000)
  assert bit_array.starts_with(welcome, bit_array.from_string("You are listening on"))

  // The message we sent should come through
  let assert Ok(message) = process.receive(from: chunk_subj, within: 2000)
  let assert Ok(message) = bit_array.to_string(message)
  assert string.contains(message, "x-test-header")
  assert string.contains(message, "HITHERE")
  assert string.contains(message, "Message here")
}

fn default_peer() {
  neohook.LocalPeer(
    name: "default",
    pipe_entry_subj: process.new_subject(),
    db_sync_subj: process.new_subject(),
  )
}

pub fn sse_test() {
  let ctr = counter.new_memory()
  let assert Ok(master) = pipemaster.new(ctr)
  let db = turso_connection()
  let state = neohook.AppState(
    master: master.data,
    ulid_to_string: gulid.to_string_function(),
    ulid_from_string: gulid.from_string_function(),
    db:,
    self: default_peer(),
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
      on_sse: fn(x) { process.send(subj, x) Ok(Nil) },
      on_chunk: no_chunk,
    ))

  let response.Response(status:, headers: _, body:) = neohook.http_handler(req, state)
  should.equal(status, 200)

  let assert mist.ServerSentEvents = body

  // Send something
  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_path("/test/1")
    |> request.set_header("user-agent", "curl/8.0.0")
    |> request.set_header("x-test-header", "HITHERE")
    |> request.set_body(http_wrapper.SimpleBody(bit_array.from_string("Message here"), on_sse: no_sse, on_chunk: no_chunk))

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

pub fn persisted_test() {
  let ctr = counter.new_memory()
  let assert Ok(master) = pipemaster.new(ctr)
  let db = turso_connection()
  let state = neohook.AppState(
    master: master.data,
    ulid_to_string: gulid.to_string_function(),
    ulid_from_string: gulid.from_string_function(),
    db:,
    self: default_peer(),
    peers: [],
  )

  // Send something
  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_path("/test/1")
    |> request.set_header("user-agent", "curl/8.0.0")
    |> request.set_header("x-test-header", "HITHERE")
    |> request.set_body(http_wrapper.SimpleBody(bit_array.from_string("First"), on_sse: no_sse, on_chunk: no_chunk))

  let assert response.Response(status: 200, headers: _, body: _) = neohook.http_handler(req, state)

  // It should *not* be saved to the DB
  let assert [] = fetch_entries(for: "test/1", from: state)

  update_flags(
    state,
    "test/1",
    persisted: True,
  )

  // Send something else
  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_path("/test/1")
    |> request.set_header("user-agent", "curl/8.0.0")
    |> request.set_header("x-test-header", "HITHERE")
    |> request.set_body(http_wrapper.SimpleBody(bit_array.from_string("Second"), on_sse: no_sse, on_chunk: no_chunk))

  let assert response.Response(status: 200, headers: _, body: _) = neohook.http_handler(req, state)

  // This one *should* be saved to the DB
  let assert [Entry(body: <<"Second":utf8>>, ..)] = fetch_entries(for: "test/1", from: state)

  // One more test to make sure disabling works:
  update_flags(
    state,
    "test/1",
    persisted: False,
  )
  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_path("/test/1")
    |> request.set_header("user-agent", "curl/8.0.0")
    |> request.set_header("x-test-header", "HITHERE")
    |> request.set_body(http_wrapper.SimpleBody(bit_array.from_string("Third"), on_sse: no_sse, on_chunk: no_chunk))

  let assert response.Response(status: 200, headers: _, body: _) = neohook.http_handler(req, state)

  // Should still only be one saved entry
  let assert [Entry(body: <<"Second":utf8>>, ..)] = fetch_entries(for: "test/1", from: state)

  Nil
}

pub fn peer_test() {
  let ctr = counter.new_memory()
  let assert Ok(master1) = pipemaster.new(ctr)
  let assert Ok(master2) = pipemaster.new(ctr)
  let db1 = turso_connection()
  let db2 = turso_connection()

  let peer1 = neohook.LocalPeer(
    name: "peer1",
    pipe_entry_subj: process.new_subject(),
    db_sync_subj: process.new_subject(),
  )

  let peer2 = neohook.LocalPeer(
    name: "peer2",
    pipe_entry_subj: process.new_subject(),
    db_sync_subj: process.new_subject(),
  )

  let state1 = neohook.AppState(
    master: master1.data,
    ulid_to_string: gulid.to_string_function(),
    ulid_from_string: gulid.from_string_function(),
    db: db1,
    self: peer1,
    peers: [peer2],
  )

  let state2 = neohook.AppState(
    master: master2.data,
    ulid_to_string: gulid.to_string_function(),
    ulid_from_string: gulid.from_string_function(),
    db: db2,
    self: peer2,
    peers: [peer1],
  )

  update_flags(
    state1,
    "test/1",
    persisted: True,
  )

  // Send something with state1
  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_path("/test/1")
    |> request.set_header("user-agent", "curl/8.0.0")
    |> request.set_header("x-test-header", "HITHERE")
    |> request.set_body(http_wrapper.SimpleBody(bit_array.from_string("Sent to 1"), on_sse: no_sse, on_chunk: no_chunk))

  let response.Response(status:, headers: _, body: _) = neohook.http_handler(req, state1)
  should.equal(status, 200)

  // It should appear in its own DB
  let assert [Entry(id: id_in_state1, body:)] = fetch_entries(for: "test/1", from: state1)

  should.equal(body, bit_array.from_string("Sent to 1"))

  // It should appear in state2's DB too
  process_db_syncs(peer2, db2) |> should.not_equal(0)
  process_db_syncs(peer1, db1)
  process_db_syncs(peer2, db2)
  process_db_syncs(peer1, db1)

  let assert [Entry(id: id_in_state2, body:)] = fetch_entries(for: "test/1", from: state2)

  should.equal(body, bit_array.from_string("Sent to 1"))
  should.equal(id_in_state2, id_in_state1)

  // Other way around should work too
  // (tests sync of persistence setting)

  // Send something with state2
  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_path("/test/1")
    |> request.set_header("user-agent", "curl/8.0.0")
    |> request.set_body(http_wrapper.SimpleBody(bit_array.from_string("Bod"), on_sse: no_sse, on_chunk: no_chunk))

  let assert response.Response(status: 200, headers: _, body: _) = neohook.http_handler(req, state2)

  // It should appear in state2's DB too
  process_db_syncs(peer1, db1)
  process_db_syncs(peer2, db2)
  process_db_syncs(peer1, db1)
  process_db_syncs(peer2, db2)

  let assert [Entry(
    body: <<"Bod":utf8>>,
    ..
  ), ..] = fetch_entries(for: "test/1", from: state1)
  Nil
}

// This tests a case where given the naive implementation of sending pipe entries
// to each peer every single time can result in some peers missing old entries.
// Specifically, this happens when one peer is down then comes back up, then a
// new entry gets inserted before tell_nodes_where_were_at gets run.
pub fn peer_bug_test() {
  let ctr = counter.new_memory()
  let assert Ok(master1) = pipemaster.new(ctr)
  let assert Ok(master2) = pipemaster.new(ctr)
  let db1 = turso_connection()
  let db2 = turso_connection()

  let peer1 = neohook.LocalPeer(
    name: "peer1",
    pipe_entry_subj: process.new_subject(),
    db_sync_subj: process.new_subject(),
  )

  let peer2 = neohook.LocalPeer(
    name: "peer2",
    pipe_entry_subj: process.new_subject(),
    db_sync_subj: process.new_subject(),
  )

  let state1 = neohook.AppState(
    master: master1.data,
    ulid_to_string: gulid.to_string_function(),
    ulid_from_string: gulid.from_string_function(),
    db: db1,
    self: peer1,
    peers: [peer2],
  )

  let state2 = neohook.AppState(
    master: master2.data,
    ulid_to_string: gulid.to_string_function(),
    ulid_from_string: gulid.from_string_function(),
    db: db2,
    self: peer2,
    peers: [peer1],
  )

  update_flags(
    state1,
    "test/1",
    persisted: True,
  )

  // Send something with peer1
  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_path("/test/1")
    |> request.set_header("user-agent", "curl/8.0.0")
    |> request.set_header("x-test-header", "HITHERE")
    |> request.set_body(http_wrapper.SimpleBody(bit_array.from_string("Sent to 1"), on_sse: no_sse, on_chunk: no_chunk))
  let response.Response(status:, headers: _, body: _) = neohook.http_handler(req, state1)
  should.equal(status, 200)

  // Suppose peer2 is down
  drop_db_syncs(peer2) |> should.not_equal(0)

  // Send something else with peer1
  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_path("/test/1")
    |> request.set_header("user-agent", "curl/8.0.0")
    |> request.set_body(http_wrapper.SimpleBody(bit_array.from_string("new one"), on_sse: no_sse, on_chunk: no_chunk))
  let response.Response(status:, headers: _, body: _) = neohook.http_handler(req, state1)
  should.equal(status, 200)

  // Bring peer2 back up
  process_db_syncs(peer2, db2)
  neohook.tell_nodes_where_were_at(db2, state2.peers, state2.self)
  process_db_syncs(peer1, db1)
  process_db_syncs(peer2, db2)

  // Ensure that peer2 has both entries
  let entries = fetch_entries(for: "test/1", from: state2)
  let assert [Entry(body: body2, ..), Entry(body: body1, ..)] = entries

  should.equal(body1, bit_array.from_string("Sent to 1"))
  should.equal(body2, bit_array.from_string("new one"))
}

///////////////////////
// Utility functions //
///////////////////////

type Entry {
  Entry(id: String, body: BitArray)
}

fn update_flags(
  state: neohook.AppState,
  pipe: String,
  persisted persisted: Bool,
) {
  let req_body = json.object([
    #("persisted", json.bool(persisted)),
  ])
    |> json.to_string
    |> bit_array.from_string

  let handler = shcribe.wrap(neohook.http_handler(_, state), with: shcribe_config())

  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_path("/api/pipe/settings")
    |> request.set_query([#("pipe", pipe)])
    |> request.set_body(http_wrapper.SimpleBody(req_body, on_sse: no_sse, on_chunk: no_chunk))

  let response.Response(status:, ..) = handler(req)
  should.equal(status, 204)
}

fn fetch_entries(from state: neohook.AppState, for pipe: String) -> List(Entry) {
  let req = request.new()
    |> request.set_method(http.Get)
    |> request.set_path("/api/pipe")
    |> request.set_query([#("name", pipe)])
    |> request.set_body(http_wrapper.SimpleBody(bit_array.from_string(""), on_sse: no_sse, on_chunk: no_chunk))

  let handler = shcribe.wrap(neohook.http_handler(_, state), with: shcribe_config())

  let response.Response(status:, headers: _, body:) = handler(req)
  should.equal(status, 200)

  let assert mist.Bytes(body) = body
  let entries_decoder = {
    use id <- decode.field("id", decode.string)
    use body <- decode.field("body", decode.bit_array)
    decode.success(Entry(id:, body:))
  } |> decode.list

  let decoder = {
    use entries <- decode.field("entries", entries_decoder)
    decode.success(entries)
  }

  let assert Ok(result) = json.parse_bits(bytes_tree.to_bit_array(body), decoder)
  result
}

fn shcribe_config() -> shcribe.Config(http_wrapper.Body, mist.ResponseData) {
  shcribe.Config(
    destination: shcribe.File("api-examples.md"),
    converter: shcribe.Converter(
      request: fn(body) {
        let assert http_wrapper.SimpleBody(bytes:, ..) = body
        bytes
      },
      response: fn(body) {
        let assert mist.Bytes(bytes) = body
        bytes |> bytes_tree.to_bit_array
      },
    ),
  )
}

fn turso_connection() -> pturso.Connection {
  let assert Ok(turso) = pturso.start()
  let db_id = int.random(999999) |> int.to_string
  let conn = pturso.connect(turso, "test/db" <> db_id, log_with: fn(_) { Nil })
  let assert Ok(_) = migrations.migrate(conn, migrations.all_migrations())
  conn
}

fn no_sse(_event: http_wrapper.SSEEvent) -> Result(Nil, Nil) {
  Error(Nil)
}

fn no_chunk(_data: BitArray) -> Result(Nil, Nil) {
  Error(Nil)
}

@external(erlang, "fs", "delete_files_matching")
fn delete_files_matching(pattern: String) -> Nil

fn process_db_syncs(peer: neohook.Peer, db: pturso.Connection) -> Int {
  yielder.repeatedly(fn() { neohook.peer_receive_db_sync(peer, within: 100) })
    |> yielder.take_while(result.is_ok)
    |> yielder.filter_map(function.identity)
    |> yielder.map(neohook.db_receive_loop_iter(_, db, peer))
    |> yielder.to_list
    |> list.length
}

fn drop_db_syncs(peer: neohook.Peer) -> Int {
  yielder.repeatedly(fn() { neohook.peer_receive_db_sync(peer, within: 100) })
    |> yielder.take_while(result.is_ok)
    |> yielder.filter_map(function.identity)
    |> yielder.to_list
    |> list.length
}
