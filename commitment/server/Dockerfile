FROM node:20.19.2

# Set the working directory in the container
WORKDIR /server

# Copy the rest of your app code
COPY . .

RUN npm install

# Run the app
CMD ["node", "index.js"]
