#!/bin/bash
set -e

echo ">>> Logging in to Docker Hub"
echo "${DOCKERHUB_TOKEN}" | docker login -u "${DOCKERHUB_USERNAME}" --password-stdin

echo ">>> Pulling latest images"
sudo docker compose -f docker-compose.prod.yml pull

echo ">>> Restarting containers"
sudo docker compose -f docker-compose.prod.yml down
sudo docker compose -f docker-compose.prod.yml up -d

echo ">>> Deployment complete"
