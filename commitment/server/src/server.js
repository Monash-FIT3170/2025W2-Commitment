import { MongoClient } from 'mongodb';
import express from 'express';
import cors from 'cors';
import { 
    ATLAS_MONGODB_URI,
    SERVER_HOST,
    SERVER_PORT 
} from './constants.js';

// Add detailed startup logging
console.log('Starting server initialization...');
console.log('Current working directory:', process.cwd());
console.log('Environment variables:', {
    SERVER_HOST: SERVER_HOST || 'not set',
    SERVER_PORT: SERVER_PORT || 'not set',
    ATLAS_MONGODB_URI: ATLAS_MONGODB_URI ? 'URI is set' : 'URI is not set'
});

if (!ATLAS_MONGODB_URI) {
    console.error('ATLAS_MONGODB_URI is not set in environment variables');
    process.exit(1);
}

if (!SERVER_HOST || !SERVER_PORT) {
    console.error('SERVER_HOST or SERVER_PORT is not set in environment variables');
    process.exit(1);
}

// Initialize Express app
const app = express();
app.use(cors());
app.use(express.json());

// Test endpoint
app.get('/', (req, res) => {
    res.json({ message: 'Server is running!' });
});

// MongoDB connection setup
const client = new MongoClient(ATLAS_MONGODB_URI, {
    ssl: true,
    tls: true,
    tlsAllowInvalidCertificates: true,
    tlsAllowInvalidHostnames: true,
    minPoolSize: 1,
    maxPoolSize: 10,
    serverApi: {
        version: '1',
        strict: true,
        deprecationErrors: true,
    },
    directConnection: false,
    retryWrites: true,
    retryReads: true
});

// Database and collections
let db;
const USERS_COLLECTION = 'users';
const REPOS_COLLECTION = 'repositories';

// API Routes
app.post('/api/users/register', async (req, res) => {
    try {
        const { username, email, password } = req.body;
        
        // Check if user already exists
        const existingUser = await db.collection(USERS_COLLECTION).findOne({ email });
        if (existingUser) {
            return res.status(400).json({ error: 'User already exists' });
        }

        // Create new user
        const newUser = {
            username,
            email,
            password, // Note: In production, you should hash the password
            createdAt: new Date(),
            updatedAt: new Date()
        };

        const result = await db.collection(USERS_COLLECTION).insertOne(newUser);
        res.status(201).json({ 
            message: 'User created successfully',
            userId: result.insertedId 
        });
    } catch (error) {
        console.error('Registration error:', error);
        res.status(500).json({ error: 'Failed to register user' });
    }
});

app.post('/api/users/login', async (req, res) => {
    try {
        const { email, password } = req.body;
        
        const user = await db.collection(USERS_COLLECTION).findOne({ email, password });
        if (!user) {
            return res.status(401).json({ error: 'Invalid credentials' });
        }

        res.json({ 
            message: 'Login successful',
            userId: user._id,
            username: user.username
        });
    } catch (error) {
        console.error('Login error:', error);
        res.status(500).json({ error: 'Failed to login' });
    }
});

app.post('/api/repos', async (req, res) => {
    try {
        const { userId, repoName, repoUrl, description } = req.body;
        
        const newRepo = {
            userId,
            repoName,
            repoUrl,
            description,
            createdAt: new Date(),
            updatedAt: new Date()
        };

        const result = await db.collection(REPOS_COLLECTION).insertOne(newRepo);
        res.status(201).json({ 
            message: 'Repository added successfully',
            repoId: result.insertedId 
        });
    } catch (error) {
        console.error('Repository creation error:', error);
        res.status(500).json({ error: 'Failed to add repository' });
    }
});

app.get('/api/repos/:userId', async (req, res) => {
    try {
        const { userId } = req.params;
        const repos = await db.collection(REPOS_COLLECTION)
            .find({ userId })
            .toArray();
        
        res.json(repos);
    } catch (error) {
        console.error('Repository fetch error:', error);
        res.status(500).json({ error: 'Failed to fetch repositories' });
    }
});

// Initialize server
async function main() {
    try {
        console.log('Attempting to connect to MongoDB...');
        await client.connect();
        console.log('Connected to MongoDB Atlas');
        
        db = client.db();
        console.log('Database connection established');
        
        // Create indexes
        console.log('Creating database indexes...');
        await db.collection(USERS_COLLECTION).createIndex({ email: 1 }, { unique: true });
        await db.collection(REPOS_COLLECTION).createIndex({ userId: 1 });
        console.log('Database indexes created successfully');
        
        app.listen(SERVER_PORT, '0.0.0.0', () => {
            console.log(`Server listening on 0.0.0.0:${SERVER_PORT}`);
        });
    } catch (err) {
        console.error('Connection error:', err);
        process.exit(1);
    }
}

// Handle process termination
process.on('SIGINT', async () => {
    try {
        await client.close();
        console.log('MongoDB connection closed.');
        process.exit(0);
    } catch (err) {
        console.error('Error closing MongoDB connection:', err);
        process.exit(1);
    }
});

main()
    .then(() => console.log('Server started'))
    .catch(err => {
        console.error('Startup error:', err);
        process.exit(1);
    });