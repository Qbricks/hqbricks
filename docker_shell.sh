#!/usr/bin/env bash
set -e

IMAGE="hqbricks:latest"
IMAGES_TAR="hqbricks_images.tar.gz"
DOCKERFILE="Dockerfile.hqbricks"

# Load image if needed
if ! docker image inspect "$IMAGE" >/dev/null 2>&1; then
	if [ -f "$IMAGES_TAR" ]; then
		echo "Loading Docker images..."
		docker load < "$IMAGES_TAR"
	else
		echo "Building Docker image..."
		docker build -f "$DOCKERFILE" -t "$IMAGE" .
	fi
fi

# Create data dir
mkdir -p data
chmod -R 775 data

# Run container
docker run --rm -it -v "$(pwd)/data:/hqbricks/data:rw" "$IMAGE" bash
