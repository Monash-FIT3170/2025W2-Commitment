import { Meteor } from 'meteor/meteor';
import { BookmarksCollection } from '../imports/api/bookmarks';
import '../imports/api/bookmark_methods';

Meteor.startup(async () => {
  // We publish the entire Bookmarks collection to all clients.
  // In order to be fetched in real-time to the clients
  Meteor.publish("bookmarks", function () {
    return BookmarksCollection.find();
  });
});
