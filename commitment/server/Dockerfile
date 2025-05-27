# Start with Node.js base image
FROM node:18.19.1-slim

# Install curl, git, and other dependencies
RUN apt-get update && apt-get install -y \
    curl \
    git \
    gnupg \
    && rm -rf /var/lib/apt/lists/*

# Create app directory and set permissions
RUN mkdir -p /server && \
    chown -R node:node /server

# Set working directory
WORKDIR /server

# Set environment variables
ENV NODE_ENV=development \
    NPM_CONFIG_PREFIX=/home/node/.npm-global

# Switch to non-root user
USER node

# Create necessary directories with correct permissions
RUN mkdir -p /home/node/.npm-global && \
    chown -R node:node /home/node

# Set npm global prefix
ENV PATH=/home/node/.npm-global/bin:$PATH

# Copy package files
COPY --chown=node:node package*.json ./

# Install dependencies
RUN npm install

# Copy the rest of the application
COPY --chown=node:node . .

# Create .env file if it doesn't exist
RUN touch .env && \
    chown node:node .env && \
    chmod 644 .env

# Expose port
EXPOSE 3000

# Start command
CMD ["node", "src/server.js"]
