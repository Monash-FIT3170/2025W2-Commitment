import dotenv from 'dotenv';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import path from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Load .env file from the server root directory
dotenv.config({ path: path.join(__dirname, '../.env') });

export const DATABASE_USERNAME = process.env.MONGO_INITDB_ROOT_USERNAME;
export const DATABASE_PASSWORD = process.env.MONGO_INITDB_ROOT_PASSWORD;
export const DATABASE_DB = process.env.MONGO_INITDB_ROOT_DATABASE;
export const DATABASE_HOST = process.env.MONGO_INITDB_ROOT_HOST;
export const DATABASE_PORT = process.env.MONGO_INITDB_ROOT_PORT;
export const DATABASE_COLLECTION = process.env.MONGO_INITDB_ROOT_COLLECTION;

export const SERVER_HOST = process.env.SERVER_HOST;
export const SERVER_PORT = process.env.SERVER_PORT;

export const ATLAS_MONGODB_URI = process.env.ATLAS_MONGODB_URI;

console.log('Loading environment variables...');
console.log('Environment variables loaded:', {
    DATABASE_USERNAME,
    DATABASE_HOST,
    DATABASE_PORT,
    SERVER_HOST,
    SERVER_PORT,
    ATLAS_MONGODB_URI
});