import {MongoClient} from 'mongodb';
import http from 'http';
import { DATABASE_USERNAME } from './constants'

// const DATABASE_USERNAME = process.env.MONGO.INITDB_ROOT_USERNAME;
// const DATABASE_PASSWORD = process.env.MONGO.INITDB_ROOT_PASSWORD;
// const DATABASE_DB = process.env.MONGO.INITDB_ROOT_DATABASE;
// const DATABASE_HOST = process.env.MONGO.INITDB_ROOT_HOST;
// const DATABASE_PORT = process.env.MONGO.INITDB_ROOT_PORT;
// const DATABASE_COLLECTION = process.env.MONGO.INITDB_ROOT_COLLECTION;

// const SERVER_HOST = process.env.SERVER_HOST;
// const SERVER_PORT = process.env.SERVER_PORT;

// connection URI
const URI = `mongodb://${DATABASE_USERNAME}:${DATABASE_PASSWORD}@{DATABASE_HOST}:${DATABASE_PORT}`;
const client = new MongoClient(URI);
const db = client.db(DATABASE_DB);
const collection = db.collection(DATABASE_COLLECTION);

console.log('hello')

// try to connect to mongo
async function main() {
    try {
        //conect to Mongo
        await client.connect();
        console.log('Connected to Mongo');
        // create server and listen for requests
        const server = http.createServer();
        // Query Mongo
        server.on('request', async (req, res) => {
            const result = await collection.findOne();
            res.end(JSON.stringify(result));
        });
        server.listen(SERVER_PORT,SERVER_HOST);
    } catch (err) {
        console.error('Something went wrong',err)
    }
}

main()
    .then(() => console.log('Server started'))
    .catch(err => console.error('Something went wrong',err));