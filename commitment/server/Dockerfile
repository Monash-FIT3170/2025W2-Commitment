# FROM node:20-alpine
# WORKDIR /server
# # COPY package*.json .
# COPY . /server
# RUN npm i
# CMD ["node", "index.js"]
FROM node:20

# Set the working directory in the container
WORKDIR /server

# Copy package.json and install dependencies
COPY package*.json ./
RUN npm install

# Copy the rest of your app code
COPY . .

# Run the app
CMD ["node", "index.js"]
