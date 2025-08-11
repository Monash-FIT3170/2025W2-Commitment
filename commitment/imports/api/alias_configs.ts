import { Mongo } from 'meteor/mongo';

/**
 * Structure for a single student's mapping.
 */
export interface StudentAlias {
  officialName: string;    // Student's official name for reports.
  gitUsernames: string[];  // Array of their git usernames.
  emails: string[];        // Array of their git commit emails.
}

/**
 * Structure of the config file to be stored in the database, contains all student mappings
 */
export interface AliasConfig {
  _id?: string;              // Optional unique ID, generated via mongo
  name: string;              // Name for the config file
  ownerId: string;           // Meteor user ID of the owner of the config file
  createdAt: Date;           // Date config file was created
  aliases: StudentAlias[];   // Array of the student aliases in the form above
}

// Creating the mongo collection where the aliasconfig will be stored
export const AliasConfigsCollection = new Mongo.Collection<AliasConfig>('aliasConfigs');