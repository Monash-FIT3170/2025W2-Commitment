# Start with Node.js base image
FROM node:current

# Install curl and other dependencies
RUN apt-get update && apt-get install -y curl git && rm -rf /var/lib/apt/lists/*

# Install Meteor 3.2 (allowing superuser)
RUN METEOR_ALLOW_SUPERUSER=true curl https://install.meteor.com/ | sh

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