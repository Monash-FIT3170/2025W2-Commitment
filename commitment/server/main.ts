import { Meteor } from "meteor/meteor";
import { BookmarksCollection } from "../imports/api/bookmarks";
import { RepositoriesCollection } from "../imports/api/repositories";

import "../imports/api/methods";
import "../imports/api/bookmark_methods";
import "../imports/api/repository_methods";
import "../imports/api/alias_config_methods";
import "./fetch_repo";
import "./caching";
import "./methods";

Meteor.startup(() => {
  Meteor.publish("bookmarks", function () {
    if (!this.userId) {
      return this.ready(); // Return empty cursor if not logged in
    }
    return BookmarksCollection.find({ userID: this.userId });
  });

  // Publish repositories for the current user only
  Meteor.publish("repositories", function () {
    if (!this.userId) {
      return this.ready(); // Return empty cursor if not logged in
    }
    return RepositoriesCollection.find({ userID: this.userId });
  });
});
