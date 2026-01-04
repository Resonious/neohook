import gleam/string
import gleam/list
import gleam/set
import gleam/dynamic/decode
import gleam/result.{try}
import pturso

/// Add new migrations to the end of this list.
///
/// The list order determines the order in which they run, and
/// the names are just unique identifiers so that we know what
/// already has run.
pub fn all_migrations() -> List(#(String, String)) {
  [
    #("create pipe entries", "
      CREATE TABLE pipe_entries (
        id BLOB PRIMARY KEY,
        pipe TEXT NOT NULL,
        method TEXT,
        headers JSON,
        body BLOB
      );
      CREATE INDEX pipe_entries_pipe
      ON pipe_entries (pipe, id);
    "),
  ]
}

/// When OK, returns the list of migrations successfully ran.
/// When Error, returns 3 things:
///
/// 1. List of successful migrations
/// 2. Name of migration that failed
/// 3. The error that occurred
pub fn migrate(
  conn: pturso.Connection,
  migrations: List(#(String, String)),
) {
  use _ <- try(pturso.exec(
    "CREATE TABLE IF NOT EXISTS migrations (
        name TEXT PRIMARY KEY,
        applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )",
    on: conn,
  )
    |> result.map_error(fn(e) { #([], "init1", e) })
  )

  let decoder = {
    use name <- decode.field(0, decode.string)
    decode.success(name)
  }

  use already_run <- try(pturso.query(
    "SELECT name FROM migrations",
    on: conn,
    with: [],
    expecting: decoder,
  )
    |> result.map_error(fn(e) { #([], "init2", e) })
    |> result.map(set.from_list)
  )

  let save_ran = fn(ran: List(String)) {
    case ran {
      [] -> Nil

      [_, ..] -> {
        let placeholders = list.map(ran, fn(_) { "(?)" })
          |> string.join(", ")
        let assert Ok(_) = pturso.query(
          "INSERT INTO migrations (name) VALUES (" <> placeholders <> ")",
          on: conn,
          with: list.map(ran, pturso.String),
          expecting: decode.success(Nil),
        )
        Nil
      }
    }
  }

  migrations
  |> list.filter(fn(k) { !set.contains(already_run, k.0) })
  |> list.fold_until(Ok([]), fn(ran, migration) {
    let #(name, sql) = migration
    case pturso.exec(sql, on: conn) {
      Ok(_) -> list.Continue(result.map(ran, list.append(_, [name])))
      Error(e) -> list.Stop(Error(#(result.unwrap(ran, []), name, e)))
    }
  })
  |> result.map(fn(x) { save_ran(x) x })
  |> result.map_error(fn(x) { save_ran(x.0) x })
}
