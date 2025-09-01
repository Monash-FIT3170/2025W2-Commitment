#!/bin/bash
set -e

APP_DIR=/home/ubuntu/app
COMPOSE_FILE=docker-compose.prod.yml

echo "Pulling latest commits!"
if [ ! -d "$APP_DIR" ]; then
  git clone https://github.com/Monash-FIT3170/2025W2-Commitment.git "$APP_DIR"
else
  cd "$APP_DIR"
  git fetch origin cd/deploy
  git reset --hard origin/cd/deploy
fi

cd "$APP_DIR"

echo "Stopping and removing existing containers..."
sudo docker compose -f $COMPOSE_FILE down --volumes --rmi all --remove-orphans

echo "Cleaning up Docker system..."
sudo docker system prune -af --volumes

echo "Building containers and running fresh!"
sudo docker compose -f $COMPOSE_FILE build --no-cache
sudo docker compose -f $COMPOSE_FILE up -d

echo "Deployment complete!"
