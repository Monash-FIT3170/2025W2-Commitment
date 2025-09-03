import { Meteor } from 'meteor/meteor';
import { check } from 'meteor/check';
import { AliasConfigsCollection, StudentAlias } from './alias_configs';

Meteor.methods({
    /**
     * Creates or replaces alias configuration (only one config per user)
     *
     * @param {string} name - User's name for this config.
     * @param {StudentAlias[]} aliases - Array of studentAlias objects.
     * @returns {Promise<string>} ID of the config (new or updated)
     * @throws {Meteor.Error} If user is not logged in/not a teacher or admin/or for duplicates or incorrect config file structure
     */
    async 'aliasConfigs.create'(name: string, aliases: StudentAlias[]) {
        check(name, String);
        check(aliases, Array);

        // Ensure user is logged in 
        if (!this.userId) {
            throw new Meteor.Error('not-authorized', 'You must be logged in to create a configuration.');
        }

        // TODO: add check to check that the logged in user has the appropriate role, currently just checking if they're logged in

        // Error Checking: check student aliases (user's config file) for duplicates (git username or email)
        const seenIdentifiers = new Set<string>(); // stores unique usernames/emails
        for (const student of aliases) { // loop through student aliases
            const identifiers = [...student.gitUsernames, ...student.emails]; // combines emails + usernames
            for (const id of identifiers) { // loops through identifiers and either adds to seen identifiers, or throws error if duplicates
                if (seenIdentifiers.has(id.toLowerCase())) { // duplicate
                    throw new Meteor.Error('duplicate-identifier', `The identifier "${id}" is assigned to more than one student.`);
                }
                seenIdentifiers.add(id.toLowerCase()); // add to identifier
            }
        }
        
        // Check if user already has a config and remove it
        const existingConfigs = await AliasConfigsCollection.find({ ownerId: this.userId }).fetchAsync();
        if (existingConfigs.length > 0) {
            // Remove all existing configs for this user (should only be one, but just in case)
            await AliasConfigsCollection.removeAsync({ ownerId: this.userId });
        }
        
        // Create new config
        const newConfig = {
            name,
            aliases,
            ownerId: this.userId, // Match config to the current logged-in user
            createdAt: new Date(),
        };

        return await AliasConfigsCollection.insertAsync(newConfig);
    },

    /**
     * Remove a config file
     *
     * @param {string} configId - The ID of the config file to remove
     */
    async 'aliasConfigs.remove'(configId: string) {
        check(configId, String);

        if (!this.userId) {
            throw new Meteor.Error('not-authorized', 'You must be logged in.');
        }

        // Try and remove based on curreent userID
        const removeConfig = await AliasConfigsCollection.removeAsync({
            _id: configId,
            ownerId: this.userId,
        });

        // no removal means no config or wrong permissions
        if (removeConfig == 0) {
            throw new Meteor.Error('not-found', 'Config file not found, or no permission to remove it.');
        }
        return removeConfig;
    },

    /**
     * Returns all config files for a single user
     */
    async 'aliasConfigs.getAllForOwner'() {
        if (!this.userId) {
            throw new Meteor.Error('not-authorized', 'You must be logged in.');
        }
        // Finds and returns all alias config files where current user is their owner
        return AliasConfigsCollection.find({ ownerId: this.userId }).fetchAsync();

    },
});