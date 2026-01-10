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

-- name: pipe_settings_from_node_since :many
SELECT *
FROM pipe_settings
WHERE node = ?
AND id > ?;

-- name: latest_pipe_settings_by_node :many
select node, max(id) as latest_id
from pipe_settings
group by node;
