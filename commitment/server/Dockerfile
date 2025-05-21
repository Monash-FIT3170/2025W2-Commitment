FROM node:20.19.2

# Set the working directory in the container
WORKDIR /server

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm install

# Copy the rest of your app code
COPY . .

# Expose the port the app runs on
EXPOSE 3000

# Run the app
CMD ["node", "src/server.js"]
