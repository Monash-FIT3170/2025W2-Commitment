import mongoose from 'mongoose';
import { ATLAS_MONGODB_URI } from '../constants.js';

export async function listCollections() {
    console.log('Attempting to connect to MongoDB Atlas with URI:', ATLAS_MONGODB_URI);
    try {
        await mongoose.connect(ATLAS_MONGODB_URI);
        console.log('Connected to MongoDB Atlas');

        const db = mongoose.connection.db;
        const collections = await db.listCollections().toArray();

        console.log('\nCollections in the database:');
        collections.forEach(collection => {
            console.log(`- ${collection.name}`);
        });

        // Close the connection
        await mongoose.connection.close();
        console.log('\nConnection closed');
        
        return collections;
    }
    catch(err) {
        console.error('Error:', err);
        // Make sure to close the connection even if there's an error
        try {
            await mongoose.connection.close();
        } catch (closeErr) {
            console.error('Error closing connection:', closeErr);
        }
        throw err; // Re-throw the error to be handled by the caller
    }
}

// Actually run the function
listCollections();