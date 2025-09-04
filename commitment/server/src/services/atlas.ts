import { MongoClient, Db } from 'mongodb';
import dotenv from 'dotenv';

// Load environment variables
dotenv.config();

const MONGODB_URI = process.env.MONGODB_URI || 'mongodb://localhost:27017';
const DB_NAME = process.env.DB_NAME || 'commitment_db';

let client: MongoClient | null = null;
let db: Db | null = null;

/**
 * Connect to MongoDB Atlas
 */
export async function connectToDatabase() {
  try {
    if (!client) {
      client = new MongoClient(MONGODB_URI);
      await client.connect();
      console.log('✅ Connected to MongoDB Atlas');
      
      db = client.db(DB_NAME);
      
      // Create collections if they don't exist
      await db.createCollection('repositories');
      await db.createCollection('commits');
      await db.createCollection('contributors');
      
      console.log('✅ Database collections initialized');
    }
    return db;
  } catch (error) {
    console.error('❌ Error connecting to MongoDB:', error);
    throw error;
  }
}

/**
 * Get database instance
 */
export async function getDatabase(): Promise<Db> {
  if (!db) {
    await connectToDatabase();
  }
  if (!db) {
    throw new Error('Failed to connect to database');
  }
  return db;
}

/**
 * Close database connection
 */
export async function closeConnection() {
  if (client) {
    await client.close();
    client = null;
    db = null;
    console.log('✅ Database connection closed');
  }
}

/**
 * Cache repository data
 */
export async function cacheRepositoryData(url: string, data: any) {
  try {
    const db = await getDatabase();
    const collection = db.collection('repositories');
    
    // Check if repository already exists
    const existing = await collection.findOne({ url });
    
    if (existing) {
      // Update existing repository
      await collection.updateOne(
        { url },
        { 
          $set: { 
            ...data,
            updatedAt: new Date()
          }
        }
      );
      console.log(`✅ Updated repository: ${url}`);
    } else {
      // Insert new repository
      await collection.insertOne({
        url,
        ...data,
        createdAt: new Date(),
        updatedAt: new Date()
      });
      console.log(`✅ Cached new repository: ${url}`);
    }
    
    return true;
  } catch (error) {
    console.error('❌ Error caching repository data:', error);
    return false;
  }
}

/**
 * Get repository data from cache
 */
export async function getRepositoryData(url: string) {
  try {
    const db = await getDatabase();
    const collection = db.collection('repositories');
    
    const data = await collection.findOne({ url });
    return data;
  } catch (error) {
    console.error('❌ Error getting repository data:', error);
    return null;
  }
}

/**
 * Check if repository exists in database
 */
export async function isRepositoryCached(url: string) {
  try {
    const db = await getDatabase();
    const collection = db.collection('repositories');
    
    const count = await collection.countDocuments({ url });
    return count > 0;
  } catch (error) {
    console.error('❌ Error checking repository cache:', error);
    return false;
  }
}

/**
 * List all collections in the database
 */
export async function listCollections() {
  try {
    const db = await getDatabase();
    const collections = await db.listCollections().toArray();
    console.log('📋 Available collections:', collections.map((c: any) => c.name));
    return collections;
  } catch (error) {
    console.error('❌ Error listing collections:', error);
    throw error;
  }
}

/**
 * Get repository statistics
 */
export async function getRepositoryStats() {
  try {
    const db = await getDatabase();
    const collection = db.collection('repositories');
    
    const totalRepos = await collection.countDocuments();
    const recentRepos = await collection.countDocuments({
      updatedAt: { $gte: new Date(Date.now() - 24 * 60 * 60 * 1000) } // Last 24 hours
    });
    
    return {
      totalRepositories: totalRepos,
      recentlyUpdated: recentRepos
    };
  } catch (error) {
    console.error('❌ Error getting repository stats:', error);
    return { totalRepositories: 0, recentlyUpdated: 0 };
  }
} 