const express = require('express');
require('dotenv').config();

const { listCollections } = require('./src/services/atlas')

const app = express();
const PORT = process.env.SERVER_PORT || 3000;

app.get('/',(req, res) => {
  res.send('Server is up and running!');

  
});

app.get('/atlas/list', (req, res) => {
  const collections = listCollections()
  res.send('listed collections')
});

app.listen(PORT, () => {
  console.log(`Server listening on port ${PORT}`);
});







