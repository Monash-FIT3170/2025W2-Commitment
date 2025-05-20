import { MongoClient } from 'mongodb';
import http from 'http';
import { 
    ATLAS_MONGODB_URI,
    SERVER_HOST,
    SERVER_PORT 
} from './constants.js';

// connection URI with options
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

console.log('Attempting to connect to MongoDB Atlas...');

// try to connect to mongo
async function main() {
    try {
        //connect to Mongo
        await client.connect();
        console.log('Connected to MongoDB Atlas');
        
        // create server and listen for requests
        const server = http.createServer();
        
        // Query Mongo
        server.on('request', async (req, res) => {
            try {
                const db = client.db();
                const collections = await db.listCollections().toArray();
                res.writeHead(200, { 'Content-Type': 'application/json' });
                res.end(JSON.stringify({ collections: collections.map(c => c.name) }));
            } catch (error) {
                console.error('Request error:', error);
                res.writeHead(500, { 'Content-Type': 'application/json' });
                res.end(JSON.stringify({ error: 'Failed to list collections' }));
            }
        });
        
        server.listen(SERVER_PORT, SERVER_HOST);
        console.log(`Server listening on ${SERVER_HOST}:${SERVER_PORT}`);
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