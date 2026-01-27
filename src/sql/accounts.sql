-- name: create_account :exec
INSERT INTO accounts (id, node, updated_at)
VALUES (:id, :node, :updated_at);

-- name: add_key_to_account :exec
INSERT INTO account_keys (id, node, updated_at, account_id, jwk)
VALUES (:id, :node, :updated_at, :account_id, :jwk);

-- name: remove_key_from_account :exec
INSERT INTO account_keys (id, node, updated_at, account_id, jwk)
VALUES (:id, :node, :updated_at, :account_id, NULL);

-- name: keys_for_account :many
SELECT id, jwk
FROM account_keys ak
WHERE ak.account_id = :account_id
  AND ak.jwk IS NOT NULL
  AND ak.updated_at = (
    SELECT MAX(updated_at)
    FROM account_keys ak2
    WHERE ak2.id = ak.id AND ak2.account_id = :account_id
  );

-- name: fetch_account_key :one
SELECT jwk, account_id
FROM account_keys
WHERE id = :id
ORDER BY id DESC
LIMIT 1
