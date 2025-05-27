module.exports = {
  meteor: {
    port: 3000,
    settings: {
      mongodb: {
        url: process.env.MONGO_URL
      }
    }
  }
}; 