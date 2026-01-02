-- name: insert_pipe_entry :exec
INSERT INTO pipe_entries (
  id, method, headers, body
) VALUES (
  ?, ?, ?, ?
);
