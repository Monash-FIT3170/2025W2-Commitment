FROM node:20.19.2

# Set the working directory in the container
WORKDIR /server

# Copy package.json and install dependencies
COPY package*.json ./
RUN npm install

# Copy the rest of your app code
COPY . .

# Run the app
CMD ["node", "index.js"]
