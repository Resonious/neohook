-- name: insert_pipe_entry :exec
INSERT INTO pipe_entries (
  id, pipe, method, headers, body
) VALUES (
  ?, ?, ?, ?, ?
);

-- name: pipe_entries_by_pipe :many
SELECT id, method, headers, body
FROM pipe_entries
WHERE pipe = ?
order by id desc
limit ?
offset ?;
