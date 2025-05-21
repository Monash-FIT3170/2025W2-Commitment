// Create collections
db.createCollection('users');
db.createCollection('repositories');

// Create indexes
db.users.createIndex({ email: 1 }, { unique: true });
db.repositories.createIndex({ userId: 1 });

// Insert initial user if it doesn't exist
if (db.users.countDocuments({ email: 'jhat0005@student.monash.edu' }) === 0) {
    db.users.insertOne({
        username: 'FIT3170',
        password: 'MySecurePassword',
        email: 'jhat0005@student.monash.edu',
        createdAt: new Date(),
        updatedAt: new Date()
    });
}