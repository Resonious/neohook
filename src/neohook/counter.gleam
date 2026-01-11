import gleam/result
import gleam/option
import gleam/dict
import gleam/erlang/process
import gleam/otp/actor

pub type Counter {
  Counter(
    incr: fn(String) -> Nil,
    fetch: fn(String) -> Int,
  )
}

type MemMsg {
  Incr(String, Int)
  Fetch(String, process.Subject(Int))
}

pub fn new_memory() -> Counter {
  // No idea why this might fail to start
  let assert Ok(act) = actor.new(dict.new())
    |> actor.on_message(fn(state, msg) {
      case msg {
        Incr(name, i) -> state
          |> dict.upsert(name, fn(n) { option.unwrap(n, 0) + i })
          |> actor.continue
        Fetch(name, ret) -> {
          dict.get(state, name)
          |> result.unwrap(0)
          |> process.send(ret, _)

          actor.continue(state)
        }
      }
    })
    |> actor.start

  Counter(
    incr: fn(name) { process.send(act.data, Incr(name, 1)) },
    fetch: fn(name) { process.call(act.data, 500, Fetch(name, _)) },
  )
}
