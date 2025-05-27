import { Meteor } from 'meteor/meteor';
import { Accounts } from 'meteor/accounts-base';
import { BookmarksCollection } from '../imports/api/bookmarks';
import '../imports/api/bookmark_methods';



Meteor.startup(async () => {
  const userCount = await Meteor.users.find().countAsync();

  if (userCount === 0) {
    
    // Create a few test users
    const users = [
      {
        email: 'admin@test.com',
        password: 'password123',
        profile: { name: 'Admin User' }
      },
      {
        email: 'user@test.com', 
        password: 'password123',
        profile: { name: 'Test User' }
      },
      {
        email: 'demo@test.com',
        password: 'password123', 
        profile: { name: 'Demo User' }
      }
    ];

    users.forEach(userData => {
      const userId = Accounts.createUser({
        email: userData.email,
        password: userData.password,
        profile: userData.profile
      });
      
      console.log(`Created user: ${userData.email} with ID: ${userId}`);
    });
  }

  // We publish the entire Links collection to all clients.
  // In order to be fetched in real-time to the clients
  Meteor.publish("bookmarks", function () {
    return BookmarksCollection.find();
  });
});
