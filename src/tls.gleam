import gleam/list
import gleam/bit_array
import gleam/result
import gleam/string
import gleam/erlang/charlist
import gleam/option.{None, Some, type Option}

pub type Config {
  Config(privkey: String, fullchain: String)
}

pub type ConfigError {
  ConfigReadError
  ConfigNotUtf8
  ConfigMissingFields
}

type PartialConfig {
  PartialConfig(privkey: Option(String), fullchain: Option(String))
}

pub fn parse_config(at path: String) -> Result(Config, ConfigError) {
  let path = charlist.from_string(path)

  use contents <- result.try(
    read_file(path) |> result.replace_error(ConfigReadError),
  )
  use contents <- result.try(
    bit_array.to_string(contents) |> result.replace_error(ConfigNotUtf8),
  )

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
      Ok(Config(privkey:, fullchain:))
    }

    _ -> Error(ConfigMissingFields)
  }
}

@external(erlang, "file", "read_file")
fn read_file(path: charlist.Charlist) -> Result(BitArray, FileError)
type FileError


