import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import gleam/result

/// Opaque type representing a JWK (JSON Web Key)
pub type Jwk

/// Opaque type representing a JWT (JSON Web Token)
pub type Jwt

/// Error types for JWT operations
pub type JwtError {
  InvalidPem
  InvalidJwk
  VerificationFailed
  DecodeError(String)
}

/// Create a JWK from PEM-encoded key data (RSA or EC)
@external(erlang, "jwt_ffi", "jwk_from_pem")
pub fn jwk_from_pem(pem: String) -> Result(Jwk, JwtError)

/// Create a JWK from a map (e.g., from JSON)
@external(erlang, "jwt_ffi", "jwk_from_map")
pub fn jwk_from_map(map: Dynamic) -> Result(Jwk, JwtError)

/// Create a JWK from a JSON-encoded string
pub fn jwk_from_json(json_string: String) -> Result(Jwk, JwtError) {
  use parsed <- result.try(
    json_string
    |> json.parse(decode.dynamic)
    |> result.map_error(fn(_) { InvalidJwk }),
  )
  jwk_from_map(parsed)
}

/// Extract the public key from a JWK (works for both RSA and EC keys)
@external(erlang, "jwt_ffi", "jwk_to_public")
pub fn jwk_to_public(jwk: Jwk) -> Jwk

/// Verify a JWT using the given JWK
/// Returns the decoded claims if verification succeeds
@external(erlang, "jwt_ffi", "jwt_verify")
pub fn verify(jwt_string: String, jwk: Jwk) -> Result(Dynamic, JwtError)

/// Verify a JWT and return a tuple of (header, claims) as Dynamic
@external(erlang, "jwt_ffi", "jwt_verify_full")
pub fn verify_full(jwt_string: String, jwk: Jwk) -> Result(#(Dynamic, Dynamic), JwtError)

/// Sign a payload to create a JWT
@external(erlang, "jwt_ffi", "jwt_sign")
pub fn sign(claims: Dynamic, jwk: Jwk) -> Result(String, JwtError)

/// Sign with a specific algorithm
@external(erlang, "jwt_ffi", "jwt_sign_with_alg")
pub fn sign_with_alg(
  claims: Dynamic,
  jwk: Jwk,
  algorithm: String,
) -> Result(String, JwtError)

/// Get the algorithm from a JWK
@external(erlang, "jwt_ffi", "jwk_algorithm")
pub fn jwk_algorithm(jwk: Jwk) -> Result(String, JwtError)

/// Convenience function: verify a JWT from PEM key
pub fn verify_with_pem(
  jwt_string: String,
  pem: String,
) -> Result(Dynamic, JwtError) {
  use jwk <- result.try(jwk_from_pem(pem))
  verify(jwt_string, jwk)
}
