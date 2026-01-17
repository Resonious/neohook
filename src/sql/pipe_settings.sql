-- name: insert_pipe_settings :exec
INSERT INTO pipe_settings (
  id, node, namespace, flags
) VALUES (
  ?, ?, ?, ?
);

-- name: latest_pipe_settings :one
SELECT * FROM pipe_settings
WHERE namespace = ?
ORDER BY id DESC
LIMIT 1;
