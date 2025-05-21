import dotenv from 'dotenv';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import path from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

console.log('Loading environment variables...');
console.log('Current directory:', __dirname);
console.log('Looking for .env file in:', path.join(__dirname, '../.env'));

// Load .env file from the server root directory
const result = dotenv.config({ path: path.join(__dirname, '../.env') });

if (result.error) {
    console.error('Error loading .env file:', result.error);
    process.exit(1);
}

console.log('Environment variables loaded successfully');

export const DATABASE_USERNAME = process.env.MONGO_INITDB_ROOT_USERNAME;
export const DATABASE_PASSWORD = process.env.MONGO_INITDB_ROOT_PASSWORD;
export const DATABASE_DB = process.env.MONGO_INITDB_ROOT_DATABASE;
export const DATABASE_HOST = process.env.MONGO_INITDB_ROOT_HOST;
export const DATABASE_PORT = process.env.MONGO_INITDB_ROOT_PORT;
export const DATABASE_COLLECTION = process.env.MONGO_INITDB_ROOT_COLLECTION;

export const SERVER_HOST = process.env.SERVER_HOST;
export const SERVER_PORT = process.env.SERVER_PORT;

export const ATLAS_MONGODB_URI = process.env.ATLAS_MONGODB_URI;

console.log('Environment variables loaded:', {
    DATABASE_USERNAME: DATABASE_USERNAME ? 'set' : 'not set',
    DATABASE_HOST: DATABASE_HOST ? 'set' : 'not set',
    DATABASE_PORT: DATABASE_PORT ? 'set' : 'not set',
    SERVER_HOST: SERVER_HOST ? 'set' : 'not set',
    SERVER_PORT: SERVER_PORT ? 'set' : 'not set',
    ATLAS_MONGODB_URI: ATLAS_MONGODB_URI ? 'set' : 'not set'
});