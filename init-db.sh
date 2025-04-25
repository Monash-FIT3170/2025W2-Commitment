#!/bin/bash

# This scipt is the manual database initialisation script for Commitment project

# This script executes the mongo-init.js file inside the running MongoDB container
# It can be used to:
#  - Initialize a fresh database
#  - Reset the database if needed
#  - Re-create collections and indexes

# Print a message to indicate the process is starting
echo "Starting MongoDB database initialisation..."

# Use docker exec to run a command inside the running MongoDB container
# Please refer to Arosh's README to run the MongoDB containter properly

docker exec -it mongo mongosh --file /docker-entrypoint-initdb.d/mongo-init.js

# Check if the command was successful
if [ $? -eq 0 ]; then
    echo "Database initialisation completed successfully!"
else
    echo "Error: Database initialisation failed."
    echo "Make sure the MongoDB container is running with: docker-compose ps"
fi

# Instructions for the team
echo ""
echo "The initialisation script has:"
echo "  - Created necessary collections (users, commitments, events, notifications)"
echo "  - Set up appropriate indexes for query optimization"
echo "  - Created an admin user (if it didn't exist)"
echo "  - Added sample data for testing (if the database was empty)"
echo ""
echo "You can now proceed with your development work!"