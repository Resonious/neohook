#!/bin/bash

set -e

if [ -z "$1" ]; then
    echo "Usage: ./deploy.sh user@host"
    exit 1
fi

gleam export erlang-shipment

TARGET="$1"

echo "Deploying to $TARGET..."

echo "Syncing build/erlang-shipment/ -> ~/neohook/"
rsync -avz --delete ./build/erlang-shipment/ "$TARGET:~/neohook/"

echo "Syncing static/ -> ~/neohook/static/"
rsync -avz ./static/ "$TARGET:~/neohook/static/"

echo "Deploy complete!"
