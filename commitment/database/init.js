db.createCollection('user');
db.user.insertOne(
    {
        username: 'Janidu',
        password: '123',
        email: 'jhat0005@student.monash.edu',
        subscribedAt: new Date()
    }
);