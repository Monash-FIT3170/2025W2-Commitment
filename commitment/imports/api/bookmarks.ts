import { Mongo } from 'meteor/mongo';

export interface Bookmark {
  _id?: string;
  title: string;
  url: string;
  createdAt: Date;
  userID: string;
}

export const BookmarksCollection = new Mongo.Collection<Bookmark>('bookmarks');
