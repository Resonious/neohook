#!/bin/bash

set -e

RESTART=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --restart)
            RESTART=true
            shift
            ;;
        *)
            TARGET="$1"
            shift
            ;;
    esac
done

if [ -z "$TARGET" ]; then
    echo "Usage: ./deploy.sh [--restart] user@host"
    echo ""
    echo "Options:"
    echo "  --restart  Full restart instead of hot reload"
    exit 1
fi

gleam export erlang-shipment

echo "Deploying to $TARGET..."

echo "Syncing build/erlang-shipment/ -> /var/www/neohook/"
rsync -avz --delete ./build/erlang-shipment/ "$TARGET:/var/www/neohook/"

echo "Copying custom entrypoint.sh..."
rsync -avz ./entrypoint.sh "$TARGET:/var/www/neohook/entrypoint.sh"

echo "Syncing static/ -> /var/www/neohook/static/"
rsync -avz ./static/ "$TARGET:/var/www/neohook/static/"

echo "Setting ownership to web:web..."
ssh "$TARGET" "sudo chown -R web:web /var/www/neohook"

if [ "$RESTART" = true ]; then
    echo "Restarting service..."
    ssh "$TARGET" "systemctl restart hook"
else
    echo "Hot reloading modules..."
    ssh "$TARGET" "cd /var/www/neohook && ./entrypoint.sh reload"
fi

echo "Deploy complete!"
