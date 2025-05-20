import express from 'express';
import dotenv from 'dotenv';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import path from 'path';
import { listCollections } from './src/services/atlas.js';

// Set up __dirname equivalent for ES modules
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Configure dotenv
dotenv.config({ path: path.join(__dirname, '.env') });

const app = express();
const PORT = process.env.SERVER_PORT || 3000;

app.get('/', (req, res) => {
  res.send('Server is up and running!');
});

app.get('/atlas/list', async (req, res) => {
  try {
    await listCollections();
    res.send('Listed collections - check server console for output');
  } catch (error) {
    console.error('Error listing collections:', error);
    res.status(500).send('Error listing collections');
  }
});

app.listen(PORT, () => {
  console.log(`Server listening on port ${PORT}`);
});