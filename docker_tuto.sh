#!/usr/bin/env bash
set -e

IMAGE="hqbricks-tuto:latest"
IMAGES_TAR="hqbricks_images.tar.gz"
DOCKERFILE="Dockerfile.hqbricks-tuto"

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

# Run container
docker run --rm -it -p 8888:8888 "$IMAGE"
