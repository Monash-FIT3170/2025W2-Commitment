import mongoose from 'mongoose';
import { ATLAS_MONGODB_URI } from '../constants.js';

export async function listCollections() {
    console.log(ATLAS_MONGODB_URI)
    try {

        await mongoose.connect(ATLAS_MONGODB_URI, {
            useNewUrlParser: true,
            useUnifiedTopology: true,
          })
          .then(() => console.log('Connected to MongoDB Atlas'))
          .catch(err => console.error('MongoDB connection error:', err));

        const db = mongoose.connection.db

        const collections = await db.listCollections().toArray()

        console.log('collections in the database')

        collections.forEach(collection => {
            console.log(`- ${collections.name}`)
            
        });
    }
    catch(err) {
        console.log(err)
    }

};