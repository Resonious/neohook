-- name: insert_pipe_entry :many
INSERT INTO pipe_entries (
  id, node, pipe, method, headers, body
)
SELECT :id, :node, :pipe, :method, :headers, :body
WHERE (
  SELECT (flags & 1) persisted
  FROM pipe_settings 
  WHERE pipe_settings.pipe = :pipe
  ORDER BY id DESC
  LIMIT 1
) = 1
RETURNING 1;

-- name: pipe_entries_by_pipe :many
SELECT id, method, headers, body
FROM pipe_entries
WHERE pipe = ?
ORDER BY id DESC
LIMIT ?
OFFSET ?;

-- name: pipe_entries_from_node_since :many
SELECT *
FROM pipe_entries
WHERE node = ?
AND id > ?;

-- name: latest_pipe_entries :many
select node, max(id) as latest_id
from pipe_entries
group by node;
