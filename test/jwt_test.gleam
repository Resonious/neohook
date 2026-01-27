import gleam/dynamic/decode
import gleeunit/should
import jwt

// Test RSA public key (PEM format)
const rsa_public_pem = "-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAr4WwmXWVm83WPrXZ14Lk
L7h/+TinN+5ktV8eGfemjEHGpC+rCdp/ol/V28n0B1x7UuQcIDYJkIKEn6OPZsZt
bYq5MBpiNKZaGYHhoVP8EKhNRSMycUacx3SMtPfCrQ/E5x9RxkAAwD4j4J8Qt+Fv
VZq3qIfVCc3Xj4x9sH1ESwb9VmwBFDxOBVRNVy3TMzhTUQ9HCPvOg1+JSZVvuM8H
vwYrdru/9vrNJEc+aZ3Quw4V/QYJPlPSjFgz+BC0Xod9LB+5vSDcac7dl/HImEpq
tPq6QGW+aSzNwacGhDzXcGEoAvHwnPQLlsCtsSbhm/rtUZQ4o4y54GTMDBxBSt6k
HQIDAQAB
-----END PUBLIC KEY-----"

// Test RSA private key (PEM format)
const rsa_private_pem = "-----BEGIN PRIVATE KEY-----
MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQCvhbCZdZWbzdY+
tdnXguQvuH/5OKc37mS1Xx4Z96aMQcakL6sJ2n+iX9XbyfQHXHtS5BwgNgmQgoSf
o49mxm1tirkwGmI0ploZgeGhU/wQqE1FIzJxRpzHdIy098KtD8TnH1HGQADAPiPg
nxC34W9Vmreoh9UJzdePjH2wfURLBv1WbAEUPE4FVE1XLdMzOFNRD0cI+86DX4lJ
lW+4zwe/Bit2u7/2+s0kRz5pndC7DhX9Bgk+U9KMWDP4ELReh30sH7m9INxpzt2X
8ciYSmq0+rpAZb5pLM3BpwaEPNdwYSgC8fCc9AuWwK2xJuGb+u1RlDijjLngZMwM
HEFK3qQdAgMBAAECggEACf9ueL9G31IRMw8+slTlVIaI25RLihcNK3uYhlgh53lT
rER+1Jfaj1AwKU3OlaDB8Y3GwFrznoSgs0sovuzsjMHLHJ9UGrWjCjLovDzBbd14
UUjY9bTU91N/NEusjhkI2TVifg1lPY4Mkx3cYOz5X4HpljLugplz/tw8yJHKEw/h
tgPWnl54lq1RM2g1veIA1sEIxBdPJPyPgB0MkmPAkT3rZ6XpMnQZ13/jWWTEbE6S
42ve8Dob/f3n2aoP76CZZPVWMH1mCN8XtzLUDQeNM+/A/rRPt/mQXBtPRLt9a5/T
MePK9PnRHiZpeiPA0OUP3odAXHPvJ9CU0ifztLMnoQKBgQDVuJxQRj+JAfFCQAHE
PSUCCJPErr7KZwvUvJgndHHfglw5coNtG5bFfYmyMLS63F3PdyJkmlkau8VJa7eg
X2bjTnl6axHrk2Khr1rpeCpiB0B7HayhJMFlio8AAA/ILpiUmAHybNL8no5ulcH0
X/6xsHZtZy0oOLCMVpPNfm78KQKBgQDSPpaFp7cHdGi+XwNhulylYDgP0cOMj0h+
ZZaTH6cWM5QGWFfPtjOGeobDwDS9MarpvOeqHrrbd9dvnkCxIQlaDOrzsP95kA2x
ouCTSdj4vDNKa2TwzzN+Udycj6Ug36HXiOgd9Iz+PSYiREAsE1PBCnF3UTNdATdB
yNGiUWrm1QKBgQCRFqnviTEaxtnNOvy4BzZ3gfEnJeZaK8iJXJsCtfeUX8BSM55r
VL5vRgFTmeMns/BvppYPrATvtCeD53+afQmxKJYpeESLI9xinVPfiXdkYz3wFr4a
C4TLdVwNCP2tzYxB3Ev+EsjNPKq/AiXQy9ZxNLXBIbkzRAD+1Mu0Rq/7uQKBgAnM
7xlV4xtO2t8c8Lxqgo0W+iYKojH4L+nsdGNxU0mep+TP03AUTzySk5umZw72tL/m
lWoBurYmVP2YA+3920C+X2lys8YH6AuCyzMyexHFPCLY9MCg+AaIbF9D9u+cXR/9
+swCrVvgn9lzEwa68+mIwhdMyrcoKlo/sT3+y5vtAoGAe18T3nGV+uRXWZcKcdMU
y+P1Nyydm+mfFUeNaJ9lL+9FvX/hp6lMf/SBvmTbGnCF7PmgJjUzCI1QoTj1Vbh0
W1/dHGywoSzzeADCez29INQVJqsmyihOEdjLgUtybD0OKJTRGpwqv5a74NYlgs6O
QeoIxMH87QWkSstekCsQ6H4=
-----END PRIVATE KEY-----"

// Test EC public key (PEM format) - P-256
const ec_public_pem = "-----BEGIN PUBLIC KEY-----
MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAE27qfMPgd5mAIH69hFDH8VAsNjZ+r
R8iA1QmtBLBWxhYTvw/dvso7CWaYwllAGCiRvtOL35c6q9YxoNU5jZRV9g==
-----END PUBLIC KEY-----"

// Test EC private key (PEM format) - P-256
const ec_private_pem = "-----BEGIN EC PRIVATE KEY-----
MHcCAQEEIFcHYlBrwjIE36WDIUXSsa3DK/aS77cnFHDiZ20BauUQoAoGCCqGSM49
AwEHoUQDQgAE27qfMPgd5mAIH69hFDH8VAsNjZ+rR8iA1QmtBLBWxhYTvw/dvso7
CWaYwllAGCiRvtOL35c6q9YxoNU5jZRV9g==
-----END EC PRIVATE KEY-----"

// EC public key in JWK JSON format (same key as ec_public_pem)
pub const ec_public_jwk_json = "{\"kty\":\"EC\",\"x\":\"27qfMPgd5mAIH69hFDH8VAsNjZ-rR8iA1QmtBLBWxhY\",\"y\":\"E78P3b7KOwlmmMJZQBgokb7Ti9-XOqvWMaDVOY2UVfY\",\"crv\":\"P-256\"}"

// Pre-generated RS256 JWT signed with rsa_private_pem
// Claims: {"sub":"1234567890","name":"Test User","iat":1516239022}
const rsa_jwt = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IlRlc3QgVXNlciIsImlhdCI6MTUxNjIzOTAyMn0.cPQDLApL7NOnkV-nytHZ_HEASVTN73IoGAYgpa8fQa41UdgV1W0YmqHuvpqGtVk894DMi0i6PQXi-Dkar3KUNYN_X4-DkrhTmsjVEhiiHdu8JZ0D9_IRplRmlouPYLIgRTR1j0tZbleA_xDFlqvNi5CRGJKO0WV7nwbgeiImcT-_8avSjkGMz_1BRS6-Ne7GDPaouf0BUJDwQ2YvD5VuC1RggKokni8_YGakoS-CNhiJtSIMA8YjXRJMO4W7tBTIw838LovYXDk3W_ncrgUp1ZEkVvrZwqf74eM6MtCsdswOEcDs4K0MhBO1TTchPRXMvO6ksDVV4ePgnhUKy0gTDQ"

// Pre-generated ES256 JWT signed with ec_private_pem
// Claims: {"sub":"9876543210","name":"EC Test User","iat":1516239022}
const ec_jwt = "eyJhbGciOiJFUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiI5ODc2NTQzMjEwIiwibmFtZSI6IkVDIFRlc3QgVXNlciIsImlhdCI6MTUxNjIzOTAyMn0.Vi-qwODxYCKoCkfDo7bpoRifNBSyIAbK5XZen65XrCAyMQZ6TpUbLRLn027sGCWHky0OzpHAC-nsZ8a0TxIDgw"

// ============================================================================
// RSA Tests
// ============================================================================

pub fn rsa_jwk_from_pem_test() {
  let result = jwt.jwk_from_pem(rsa_public_pem)
  should.be_ok(result)
}

pub fn rsa_verify_jwt_with_public_key_test() {
  let assert Ok(jwk) = jwt.jwk_from_pem(rsa_public_pem)
  let result = jwt.verify(rsa_jwt, jwk)
  should.be_ok(result)
}

pub fn rsa_verify_jwt_with_private_key_test() {
  // Should also work with private key (contains public key info)
  let assert Ok(jwk) = jwt.jwk_from_pem(rsa_private_pem)
  let result = jwt.verify(rsa_jwt, jwk)
  should.be_ok(result)
}

pub fn rsa_verify_with_pem_convenience_test() {
  let result = jwt.verify_with_pem(rsa_jwt, rsa_public_pem)
  should.be_ok(result)
}

pub fn rsa_verify_claims_test() {
  let assert Ok(jwk) = jwt.jwk_from_pem(rsa_public_pem)
  let assert Ok(claims) = jwt.verify(rsa_jwt, jwk)

  // Verify we can decode the claims
  let sub_decoder = {
    use sub <- decode.field("sub", decode.string)
    decode.success(sub)
  }
  let assert Ok(sub) = decode.run(claims, sub_decoder)
  should.equal(sub, "1234567890")

  let name_decoder = {
    use name <- decode.field("name", decode.string)
    decode.success(name)
  }
  let assert Ok(name) = decode.run(claims, name_decoder)
  should.equal(name, "Test User")
}

pub fn rsa_invalid_signature_test() {
  // Tamper with the JWT by changing one character in the signature
  let tampered_jwt = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IlRlc3QgVXNlciIsImlhdCI6MTUxNjIzOTAyMn0.INVALID_SIGNATURE_HERE"
  let assert Ok(jwk) = jwt.jwk_from_pem(rsa_public_pem)
  let result = jwt.verify(tampered_jwt, jwk)
  should.be_error(result)
}

pub fn rsa_wrong_key_test() {
  // Try to verify RSA JWT with EC key - should fail
  let assert Ok(ec_jwk) = jwt.jwk_from_pem(ec_public_pem)
  let result = jwt.verify(rsa_jwt, ec_jwk)
  should.be_error(result)
}

// ============================================================================
// EC Tests
// ============================================================================

pub fn ec_jwk_from_pem_test() {
  let result = jwt.jwk_from_pem(ec_public_pem)
  should.be_ok(result)
}

pub fn ec_verify_jwt_with_public_key_test() {
  let assert Ok(jwk) = jwt.jwk_from_pem(ec_public_pem)
  let result = jwt.verify(ec_jwt, jwk)
  should.be_ok(result)
}

pub fn ec_verify_jwt_with_private_key_test() {
  let assert Ok(jwk) = jwt.jwk_from_pem(ec_private_pem)
  let result = jwt.verify(ec_jwt, jwk)
  should.be_ok(result)
}

pub fn ec_verify_with_pem_convenience_test() {
  let result = jwt.verify_with_pem(ec_jwt, ec_public_pem)
  should.be_ok(result)
}

pub fn ec_verify_claims_test() {
  let assert Ok(jwk) = jwt.jwk_from_pem(ec_public_pem)
  let assert Ok(claims) = jwt.verify(ec_jwt, jwk)

  let sub_decoder = {
    use sub <- decode.field("sub", decode.string)
    decode.success(sub)
  }
  let assert Ok(sub) = decode.run(claims, sub_decoder)
  should.equal(sub, "9876543210")

  let name_decoder = {
    use name <- decode.field("name", decode.string)
    decode.success(name)
  }
  let assert Ok(name) = decode.run(claims, name_decoder)
  should.equal(name, "EC Test User")
}

pub fn ec_invalid_signature_test() {
  let tampered_jwt = "eyJhbGciOiJFUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiI5ODc2NTQzMjEwIiwibmFtZSI6IkVDIFRlc3QgVXNlciIsImlhdCI6MTUxNjIzOTAyMn0.INVALID"
  let assert Ok(jwk) = jwt.jwk_from_pem(ec_public_pem)
  let result = jwt.verify(tampered_jwt, jwk)
  should.be_error(result)
}

pub fn ec_wrong_key_test() {
  // Try to verify EC JWT with RSA key - should fail
  let assert Ok(rsa_jwk) = jwt.jwk_from_pem(rsa_public_pem)
  let result = jwt.verify(ec_jwt, rsa_jwk)
  should.be_error(result)
}

// ============================================================================
// General Tests
// ============================================================================

pub fn invalid_pem_test() {
  let result = jwt.jwk_from_pem("not a valid pem")
  should.be_error(result)
}

pub fn jwk_to_public_test() {
  let assert Ok(private_jwk) = jwt.jwk_from_pem(rsa_private_pem)
  let public_jwk = jwt.jwk_to_public(private_jwk)
  // The public key should still be able to verify
  let result = jwt.verify(rsa_jwt, public_jwk)
  should.be_ok(result)
}

pub fn verify_full_returns_header_and_claims_test() {
  let assert Ok(jwk) = jwt.jwk_from_pem(rsa_public_pem)
  let assert Ok(#(header, claims)) = jwt.verify_full(rsa_jwt, jwk)

  // Check header contains alg
  let alg_decoder = {
    use alg <- decode.field("alg", decode.string)
    decode.success(alg)
  }
  let assert Ok(alg) = decode.run(header, alg_decoder)
  should.equal(alg, "RS256")

  // Check claims
  let sub_decoder = {
    use sub <- decode.field("sub", decode.string)
    decode.success(sub)
  }
  let assert Ok(sub) = decode.run(claims, sub_decoder)
  should.equal(sub, "1234567890")
}

pub fn jwk_from_json_test() {
  let result = jwt.jwk_from_json(ec_public_jwk_json)
  should.be_ok(result)
}

pub fn jwk_from_json_verify_test() {
  // Load JWK from JSON and verify a JWT with it
  let assert Ok(jwk) = jwt.jwk_from_json(ec_public_jwk_json)
  let result = jwt.verify(ec_jwt, jwk)
  should.be_ok(result)
}

pub fn jwk_from_json_invalid_test() {
  let result = jwt.jwk_from_json("not valid json")
  should.be_error(result)
}

pub fn jwk_from_json_empty_object_test() {
  let result = jwt.jwk_from_json("{}")
  should.be_error(result)
}

// ============================================================================
// Peek Tests
// ============================================================================

pub fn peek_payload_test() {
  let assert Ok(claims) = jwt.peek_payload(rsa_jwt)

  let sub_decoder = {
    use sub <- decode.field("sub", decode.string)
    decode.success(sub)
  }
  let assert Ok(sub) = decode.run(claims, sub_decoder)
  should.equal(sub, "1234567890")
}

pub fn peek_test() {
  let assert Ok(#(header, claims)) = jwt.peek(ec_jwt)

  // Check header
  let alg_decoder = {
    use alg <- decode.field("alg", decode.string)
    decode.success(alg)
  }
  let assert Ok(alg) = decode.run(header, alg_decoder)
  should.equal(alg, "ES256")

  // Check claims
  let sub_decoder = {
    use sub <- decode.field("sub", decode.string)
    decode.success(sub)
  }
  let assert Ok(sub) = decode.run(claims, sub_decoder)
  should.equal(sub, "9876543210")
}

pub fn peek_invalid_jwt_test() {
  let result = jwt.peek_payload("not.a.jwt")
  should.be_error(result)
}
