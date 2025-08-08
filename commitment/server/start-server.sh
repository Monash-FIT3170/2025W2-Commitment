#!/bin/bash

echo "🚀 Starting Commitment Server..."

# Start Docker containers
echo "📦 Starting Docker containers..."
docker compose up -d

# Wait for containers to be ready
echo "⏳ Waiting for containers to be ready..."
sleep 5

# Install dependencies
echo "📥 Installing dependencies..."
docker exec -it 3170-env bash -c "cd /projects/commitment/server && npm install tr46 cors mongodb express dotenv rxjs"

# Check if server is already running
echo "🔍 Checking if server is already running..."
if curl -s http://localhost:3000/ > /dev/null 2>&1; then
    echo "✅ Server is already running at http://localhost:3000/"
    echo "🎉 Your server is ready to use!"
else
    echo "🔥 Starting server..."
    docker exec -it 3170-env bash -c "cd /projects/commitment/server && node server.js"
fi