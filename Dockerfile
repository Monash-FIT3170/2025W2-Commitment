# Start with Node.js base image
FROM node:23-slim

# Install curl and other dependencies
RUN apt-get update && apt-get install -y curl git python3 make g++ && rm -rf /var/lib/apt/lists/* && ln -sf python3 /usr/bin/python
RUN apt-get update && apt-get install -y python3 make g++ && ln -sf python3 /usr/bin/python

# Install Meteor 3.2 (allowing superuser)
RUN METEOR_ALLOW_SUPERUSER=true curl https://install.meteor.com/ | sh

# Upgrade NPM version
RUN npm install -g npm@11.3.0

# Create a non-root user
RUN useradd -ms /bin/bash devuser

# Switch to non-root user
USER devuser

# Set working directory
WORKDIR /projects/commitment

# Expose Meteor default port
EXPOSE 3000
EXPOSE 27017

# Default command for development
CMD ["bash"]