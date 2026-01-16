-- name: insert_pipe_settings :exec
INSERT INTO pipe_settings (
  id, node, pipe, flags
) VALUES (
  ?, ?, ?, ?
);

-- name: latest_pipe_settings :one
SELECT * FROM pipe_settings
WHERE pipe = ?
ORDER BY id DESC
LIMIT 1;
