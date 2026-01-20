-module(jwt_ffi).
-export([
    jwk_from_pem/1,
    jwk_from_map/1,
    jwk_to_public/1,
    jwt_verify/2,
    jwt_verify_full/2,
    jwt_sign/2,
    jwt_sign_with_alg/3,
    jwk_algorithm/1,
    jwt_peek_payload/1,
    jwt_peek/1
]).

%% Create a JWK from PEM data
jwk_from_pem(PemBinary) when is_binary(PemBinary) ->
    try
        case jose_jwk:from_pem(PemBinary) of
            [] -> {error, invalid_pem};
            JWK -> {ok, JWK}
        end
    catch
        _:_ -> {error, invalid_pem}
    end.

%% Create a JWK from a map
jwk_from_map(Map) ->
    try
        case jose_jwk:from_map(Map) of
            {error, _} -> {error, invalid_jwk};
            JWK -> {ok, JWK}
        end
    catch
        _:_ -> {error, invalid_jwk}
    end.

%% Extract public key from JWK
jwk_to_public(JWK) ->
    jose_jwk:to_public(JWK).

%% Verify a JWT and return claims
jwt_verify(JwtString, JWK) when is_binary(JwtString) ->
    try
        case jose_jwt:verify(JWK, JwtString) of
            {true, {jose_jwt, Claims}, _JWS} ->
                {ok, Claims};
            {false, _, _} ->
                {error, verification_failed}
        end
    catch
        _:Reason ->
            {error, {decode_error, format_error(Reason)}}
    end.

%% Verify a JWT and return both header and claims
%% jose_jws record: {jose_jws, Alg, B64, Fields} where Fields is a map
jwt_verify_full(JwtString, JWK) when is_binary(JwtString) ->
    try
        case jose_jwt:verify(JWK, JwtString) of
            {true, {jose_jwt, Claims}, {jose_jws, Alg, _B64, Fields}} ->
                %% Extract the algorithm name
                AlgName = get_alg_name(Alg),
                %% Build header map with alg and any other fields
                Header = maps:put(<<"alg">>, AlgName, Fields),
                {ok, {Header, Claims}};
            {false, _, _} ->
                {error, verification_failed}
        end
    catch
        _:Reason ->
            {error, {decode_error, format_error(Reason)}}
    end.

%% Sign claims to create a JWT
jwt_sign(Claims, JWK) ->
    try
        JWT = jose_jwt:from_map(Claims),
        {_, Token} = jose_jwt:sign(JWK, JWT),
        {ok, Token}
    catch
        _:Reason ->
            {error, {decode_error, format_error(Reason)}}
    end.

%% Sign claims with a specific algorithm
jwt_sign_with_alg(Claims, JWK, Algorithm) when is_binary(Algorithm) ->
    try
        JWT = jose_jwt:from_map(Claims),
        JWS = #{<<"alg">> => Algorithm},
        {_, Token} = jose_jwt:sign(JWK, JWS, JWT),
        {ok, Token}
    catch
        _:Reason ->
            {error, {decode_error, format_error(Reason)}}
    end.

%% Get the algorithm from a JWK
jwk_algorithm(JWK) ->
    try
        {_, Map} = jose_jwk:to_map(JWK),
        case maps:get(<<"alg">>, Map, undefined) of
            undefined -> {error, {decode_error, <<"no algorithm specified">>}};
            Alg -> {ok, Alg}
        end
    catch
        _:Reason ->
            {error, {decode_error, format_error(Reason)}}
    end.

%% Helper to format error reasons
format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).

%% Helper to extract algorithm name from jose_jws_alg_* tuples
get_alg_name({jose_jws_alg_rsa_pkcs1_v1_5, Alg}) -> atom_to_binary(Alg, utf8);
get_alg_name({jose_jws_alg_rsa_pss, Alg}) -> atom_to_binary(Alg, utf8);
get_alg_name({jose_jws_alg_ecdsa, Alg}) -> atom_to_binary(Alg, utf8);
get_alg_name({jose_jws_alg_hmac, Alg}) -> atom_to_binary(Alg, utf8);
get_alg_name({jose_jws_alg_eddsa, Alg}) -> atom_to_binary(Alg, utf8);
get_alg_name(Alg) when is_atom(Alg) -> atom_to_binary(Alg, utf8);
get_alg_name(_) -> <<"unknown">>.

%% Peek at JWT payload without verifying signature
jwt_peek_payload(JwtString) when is_binary(JwtString) ->
    try
        {jose_jwt, Claims} = jose_jwt:peek_payload(JwtString),
        {ok, Claims}
    catch
        _:Reason ->
            {error, {decode_error, format_error(Reason)}}
    end.

%% Peek at both JWT header and payload without verifying signature
jwt_peek(JwtString) when is_binary(JwtString) ->
    try
        {jose_jwt, Claims} = jose_jwt:peek_payload(JwtString),
        HeaderJson = jose_jws:peek_protected(JwtString),
        Header = json:decode(HeaderJson),
        {ok, {Header, Claims}}
    catch
        _:Reason ->
            {error, {decode_error, format_error(Reason)}}
    end.
