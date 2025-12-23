#!/bin/bash

set -e

if [ -z "$1" ]; then
    echo "Usage: ./deploy.sh user@host"
    exit 1
fi

gleam export erlang-shipment

TARGET="$1"

echo "Deploying to $TARGET..."

echo "Syncing build/erlang-shipment/ -> /var/www/neohook/"
rsync -avz --delete ./build/erlang-shipment/ "$TARGET:/var/www/neohook/"

echo "Syncing static/ -> /var/www/neohook/static/"
rsync -avz ./static/ "$TARGET:/var/www/neohook/static/"

echo "Setting ownership to web:web..."
ssh "$TARGET" "sudo chown -R web:web /var/www/neohook"

ssh "$TARGET" "ssh hook systemctl restart hook"

echo "Deploy complete!"
