#!/bin/bash
set -e

echo ">>> Logging in to Docker Hub"
echo "${DOCKERHUB_TOKEN}" | docker login -u "${DOCKERHUB_USERNAME}" --password-stdin

echo ">>> Pulling latest images"
docker compose -f docker-compose.prod.yml pull

echo ">>> Restarting containers"
docker compose -f docker-compose.prod.yml down
docker compose -f docker-compose.prod.yml up -d

echo ">>> Deployment complete"
