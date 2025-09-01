#!/bin/bash
set -e

APP_DIR=/home/ubuntu/app

# Pull latest code
echo "Pulling latest commits!"
if [ ! -d "$APP_DIR" ]; then
  git clone https://github.com/Monash-FIT3170/2025W2-Commitment.git "$APP_DIR"
else
  cd "$APP_DIR"
  git fetch origin cd/deploy
  git reset --hard origin/cd/deploy
fi

cd "$APP_DIR"

# Build and run with prod config
echo "Building containers and running!"
sudo docker compose -f docker-compose.prod.yml up -d --build

echo "Deployment complete!"