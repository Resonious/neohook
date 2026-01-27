import gleam/bit_array
import gleam/dynamic
import gleam/dynamic/decode
import gleam/http/request
import gleam/list
import gleam/option
import gleam/result.{map_error, try}
import gulid.{type Ulid, type UlidError}
import jwt
import logging.{Info, log}
import neohook/http_wrapper
import neohook/sql
import pturso
import util.{parrot_to_pturso}

pub type User {
  Authenticated(account_id: Ulid, claims: Claims)
  Unauthenticated(ip_address: String)
}

pub fn to_string(user: User, ulid_to_string: fn(Ulid) -> String) -> String {
  case user {
    Authenticated(account_id:, ..) -> "acc|" <> ulid_to_string(account_id)
    Unauthenticated(ip_address:) -> "ip|" <> ip_address
  }
}

pub type Key {
  Key(id: Ulid, jwk: BitArray)
}

pub type Claims {
  Claims(
    /// Key ID
    iss: String,
    /// Permissions
    can: List(Permission),
  )
}

pub type Permission {
  Pub
  Sub
  Fetch
  Manage
}

pub fn permission_from_string(str: String) -> Result(Permission, Nil) {
  case str {
    "pub" -> Ok(Pub)
    "sub" -> Ok(Sub)
    "fetch" -> Ok(Fetch)
    "manage" -> Ok(Manage)
    _ -> Error(Nil)
  }
}

pub fn permission_decoder() -> decode.Decoder(Permission) {
  decode.string
  |> decode.then(fn(str) {
    case permission_from_string(str) {
      Ok(perm) -> decode.success(perm)
      Error(Nil) -> decode.failure(Pub, "pub|sub|fetch|manage")
    }
  })
}

pub fn claims_decoder() {
  use iss <- decode.field("iss", decode.string)
  use can <- decode.field("can", decode.list(permission_decoder()))

  decode.success(Claims(iss:, can:))
}

fn get_claims(payload: dynamic.Dynamic) {
  decode.run(payload, claims_decoder())
}

fn str(msg: String) -> fn(x) -> String {
  fn(_) { msg }
}

fn extract_authenticated_user(
  req: request.Request(discard),
  db: pturso.Connection,
  ulid_from_string: fn(String) -> Result(Ulid, UlidError),
) -> Result(User, String) {
  let auth = req.headers |> list.key_find("authorization")

  let token = case auth {
    Ok("Bearer " <> token) -> Ok(token)
    _ -> Error("no bearer token")
  }

  use token <- try(token)
  use payload <- try(jwt.peek_payload(token) |> map_error(str("malformed jwt")))
  use claims <- try(
    get_claims(payload) |> map_error(str("jwt payload missing fields")),
  )
  use key_id <- try(
    ulid_from_string(claims.iss) |> map_error(str("iss not ulid")),
  )
  use keys <- try(
    fetch_account_keys(db, key_id) |> map_error(str("internal db error")),
  )
  use key <- try(list.first(keys) |> map_error(str("no matching keys")))
  use jwk_bit <- try(option.to_result(key.jwk, "key was deleted"))
  use jwk_str <- try(
    bit_array.to_string(jwk_bit) |> map_error(str("key was not utf8")),
  )
  use jwk <- try(jwt.jwk_from_string(jwk_str) |> map_error(str("invalid jwk")))
  use _verif <- try(
    jwt.verify(token, jwk) |> map_error(str("could not verify")),
  )
  use account_id <- try(
    gulid.from_bitarray(key.account_id)
    |> map_error(str("account id malformed")),
  )

  Ok(Authenticated(account_id:, claims:))
}

fn fetch_account_keys(
  db: pturso.Connection,
  id: Ulid,
) -> Result(List(sql.FetchAccountKey), pturso.Error) {
  let #(sql, with, expecting) = sql.fetch_account_key(id: gulid.to_bitarray(id))
  let with = with |> list.map(parrot_to_pturso)
  pturso.query(sql, on: db, with:, expecting:)
}

pub fn from_request(
  req: http_wrapper.Request,
  db: pturso.Connection,
  ulid_from_string: fn(String) -> Result(Ulid, UlidError),
) -> User {
  case extract_authenticated_user(req, db, ulid_from_string) {
    Ok(u) -> u
    Error(why) -> {
      log(Info, "Unauthenticated: " <> why)
      let ip = http_wrapper.ip_address(req) |> result.unwrap("anonymous")
      Unauthenticated(ip)
    }
  }
}
