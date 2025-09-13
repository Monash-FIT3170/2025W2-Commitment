import express from 'express';
import dotenv from 'dotenv';
import { fileURLToPath } from 'url';
import path, { dirname } from 'path';
import cors from 'cors';
import { 
    listCollections, 
    connectToDatabase, 
    getRepositoryStats,
    getRepositoryData,
    isRepositoryCached
} from './services/atlas.js';

// Set up __dirname equivalent for ES modules
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Configure dotenv
dotenv.config({ path: path.join(__dirname, '../.env') });

const app = express();
const PORT = process.env.SERVER_PORT || 3000;

// Middleware
app.use(cors());
app.use(express.json());

// Initialize database connection
connectToDatabase().catch(console.error);

app.get('/', (req, res) => {
  res.send('Server is up and running!');
});

// Database health check
app.get('/health', async (req, res) => {
  try {
    await connectToDatabase();
    res.json({ status: 'healthy', message: 'Database connected' });
  } catch (error) {
    res.status(500).json({ status: 'unhealthy', error: error.message });
  }
});

// List collections
app.get('/atlas/list', async (req, res) => {
  try {
    const collections = await listCollections();
    res.json({ collections: collections.map(c => c.name) });
  } catch (error) {
    console.error('Error listing collections:', error);
    res.status(500).json({ error: 'Error listing collections' });
  }
});

// Get repository statistics
app.get('/stats', async (req, res) => {
  try {
    const stats = await getRepositoryStats();
    res.json(stats);
  } catch (error) {
    console.error('Error getting stats:', error);
    res.status(500).json({ error: 'Error getting statistics' });
  }
});

// Check if repository is cached
app.get('/repo/:url', async (req, res) => {
  try {
    const url = decodeURIComponent(req.params.url);
    const isCached = await isRepositoryCached(url);
    const data = isCached ? await getRepositoryData(url) : null;
    
    res.json({
      url,
      isCached,
      data: data ? {
        name: data.name,
        branches: data.branches,
        commitCount: Object.keys(data.allCommits || {}).length,
        contributorCount: Object.keys(data.contributors || {}).length,
        lastUpdated: data.updatedAt
      } : null
    });
  } catch (error) {
    console.error('Error checking repository:', error);
    res.status(500).json({ error: 'Error checking repository' });
  }
});

// Graceful shutdown
process.on('SIGINT', async () => {
  console.log('Shutting down server...');
  process.exit(0);
});

app.listen(PORT, () => {
  console.log(`🚀 Server listening on port ${PORT}`);
  console.log(`📊 Health check: http://localhost:${PORT}/health`);
  console.log(`📋 Collections: http://localhost:${PORT}/atlas/list`);
  console.log(`📈 Stats: http://localhost:${PORT}/stats`);
}); 