# Start with Node.js base image
FROM node:23-slim

# Install system dependencies (curl, git, build tools, etc.)
RUN apt-get update && apt-get install -y \
    curl git python3 make g++ libtinfo-dev libgmp-dev \
    tmux supervisor \
    && rm -rf /var/lib/apt/lists/* \
    && ln -sf python3 /usr/bin/python

# Install Meteor 3.2 (allowing superuser)
RUN METEOR_ALLOW_SUPERUSER=true curl https://install.meteor.com/ | sh

# Upgrade NPM version
RUN npm install -g npm@11.3.0

# --- Install Haskell Toolchain ---
RUN curl -sSL https://get.haskellstack.org/ | sh

# Create non-root user
RUN useradd -ms /bin/bash devuser

# Switch to non-root user
USER devuser

# Set working directory
WORKDIR /projects/commitment

# Copy both apps into image
COPY --chown=devuser:devuser . .

# --- Build Haskell app ---
WORKDIR /projects/commitment/api
RUN stack setup && stack build

# --- Restore main working dir for meteor ---
WORKDIR /projects/commitment

# Expose both Meteor and Haskell ports
# Meteor
# Haskell
# Mongo 
EXPOSE 3000    
EXPOSE 8081    
EXPOSE 27017   

# --- Launch both apps using a startup script ---
CMD ["bash", "run-apps.sh"]