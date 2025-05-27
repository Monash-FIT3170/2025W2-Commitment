# Start with Node.js 20 base image
FROM node:20-slim

# Install curl, git, and other dependencies
RUN apt-get update && apt-get install -y \
    curl \
    git \
    procps \
    && rm -rf /var/lib/apt/lists/*

# Create app directory and set permissions
RUN mkdir -p /projects/commitment && \
    chown -R node:node /projects

# Set working directory
WORKDIR /projects/commitment

# Create node user's home directory and set permissions
RUN mkdir -p /home/node/.meteor && \
    mkdir -p /home/node/.npm-global && \
    chown -R node:node /home/node

# Switch to node user for Meteor installation
USER node

# Install Meteor globally with proper permissions
RUN curl https://install.meteor.com/ | sh

# Set environment variables
ENV METEOR_ALLOW_SUPERUSER=true \
    METEOR_HOME=/home/node/.meteor \
    PATH=/home/node/.meteor:$PATH \
    METEOR_LOCAL_DB=0 \
    NODE_ENV=development \
    NPM_CONFIG_PREFIX=/home/node/.npm-global

# Verify Meteor installation
RUN /home/node/.meteor/meteor --version

# Create .meteor/local directory with proper permissions
RUN mkdir -p /projects/commitment/.meteor/local && \
    chmod -R 755 /projects/commitment/.meteor

# Expose port
EXPOSE 3000

# Default command
CMD ["bash"]