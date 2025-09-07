import { Mongo } from 'meteor/mongo';

export interface Repository {
  _id?: string;
  url: string;
  name: string;
  description?: string;
  owner: string;
  userID?: string; // Optional: if linked to a specific user
  createdAt: Date;
  updatedAt: Date;
  lastAnalyzed?: Date;
  analysisStatus: 'pending' | 'completed' | 'failed';
  metadata?: {
    stars?: number;
    forks?: number;
    language?: string;
    topics?: string[];
  };
}

export const RepositoriesCollection = new Mongo.Collection<Repository>('repositories');

// Create indexes for better performance
if (Meteor.isServer) {
  RepositoriesCollection.rawCollection().createIndex({ url: 1 }, { unique: true });
  RepositoriesCollection.rawCollection().createIndex({ userID: 1 });
  RepositoriesCollection.rawCollection().createIndex({ createdAt: -1 });
}
