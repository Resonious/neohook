import gleam/list
import gleam/bit_array
import gleam/string
import gleam/erlang/charlist
import gleam/option.{None, Some, type Option}

pub type Config {
  Config(privkey: String, fullchain: String)
}

type PartialConfig {
  PartialConfig(privkey: Option(String), fullchain: Option(String))
}

pub fn parse_config(at path: String) -> Config {
  let path = charlist.from_string(path)

  let assert Ok(contents) = read_file(path)
  let assert Ok(contents) = bit_array.to_string(contents)

  let lines = string.split(contents, on: "\n")

  let config = list.fold(
    lines,
    PartialConfig(None, None),
    fn(config, line) {
      case line {
        "privkey =" <> rest -> PartialConfig(
          fullchain: config.fullchain,
          privkey: Some(string.trim(rest)),
        )

        "fullchain =" <> rest -> PartialConfig(
          fullchain: Some(string.trim(rest)),
          privkey: config.privkey,
        )

        _ -> config
      }
    }
  )

  case config {
    PartialConfig(privkey: Some(privkey), fullchain: Some(fullchain)) -> {
      Config(privkey:, fullchain:)
    }

    _ -> panic as "config file did not have privkey and fullchain"
  }
}

@external(erlang, "file", "read_file")
fn read_file(path: charlist.Charlist) -> Result(BitArray, FileError)
type FileError


