// mongo-init.js
db = db.getSiblingDB('commitmentdb');

// Create collections
db.createCollection('users');

// Create indexes for better query performance
// Users collection indexes
db.users.createIndex({ "username": 1 }, { unique: true });
db.users.createIndex({ "email": 1 }, { unique: true });

// Create admin user if it doesn't exist
const adminUser = db.users.findOne({ username: "admin" });
if (!adminUser) {
    db.users.insertOne({
        username: "admin",
        email: "admin@commitment.com",
        profile: {
            name: "System Administrator",
            role: "admin"
        },
        createdAt: new Date()
    });
    print("Admin user created");
}

// Optional: Add some sample data (remove once we know storing works)
if (db.commitments.countDocuments() === 0) {
    db.commitments.insertMany([
        {
            title: "Complete Project Setup",
            description: "Set up the development environment and database infrastructure",
            userId: db.users.findOne({ username: "admin" })._id,
            status: "in-progress",
            dueDate: new Date(new Date().setDate(new Date().getDate() + 7)),
            createdAt: new Date()
        },
        {
            title: "Team Meeting",
            description: "Weekly team sync-up",
            userId: db.users.findOne({ username: "admin" })._id,
            status: "planned",
            dueDate: new Date(new Date().setDate(new Date().getDate() + 2)),
            createdAt: new Date()
        }
    ]);
    print("Sample commitments added");
}

print("Database initialisation completed");