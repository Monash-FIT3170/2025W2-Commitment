import { Meteor } from 'meteor/meteor';
import { check } from 'meteor/check';
import { RepositoriesCollection, Repository } from './repositories';

Meteor.methods({
  /**
   * Stores a new repository URL for analysis.
   * This method is called when users add a repository for analysis.
   *
   * @method repositories.storeUrl
   * @param {string} url - The GitHub repository URL.
   * @param {string} name - Repository name (extracted from URL).
   * @param {string} owner - Repository owner (extracted from URL).
   * @param {string} description - Optional description.
   * @returns {Promise<string>} The ID of the stored repository document.
   * @throws {Meteor.Error} If the URL is invalid or already exists.
   */
  async 'repositories.storeUrl'(url: string, name: string, owner: string, description?: string) {
    check(url, String);
    check(name, String);
    check(owner, String);
    if (description) check(description, String);

    if (!url.startsWith('http')) {
      throw new Meteor.Error('invalid-url', 'URL must be valid and start with http or https');
    }

    // Check if repository already exists
    const existing = await RepositoriesCollection.findOneAsync({ url });
    if (existing) {
      // Update existing repository with new analysis
      await RepositoriesCollection.updateAsync(
        { url },
        {
          $set: {
            updatedAt: new Date(),
            lastAnalyzed: new Date(),
            analysisStatus: 'pending'
          }
        }
      );
      return existing._id; // Return existing ID
    }

    // Create new repository
    const newRepo: Repository = {
      url,
      name,
      owner,
      description: description || '',
      userID: this.userId, // Link to current user if logged in
      createdAt: new Date(),
      updatedAt: new Date(),
      lastAnalyzed: new Date(),
      analysisStatus: 'pending'
    };

    return RepositoriesCollection.insertAsync(newRepo);
  },

  /**
   * Gets the most recent repositories for display.
   *
   * @method repositories.getRecent
   * @param {number} limit - Maximum number of repositories to return.
   * @returns {Promise<Repository[]>} Array of recent repositories.
   */
  async 'repositories.getRecent'(limit: number = 5) {
    check(limit, Number);

    return await RepositoriesCollection.find(
      {},
      {
        sort: { createdAt: -1 },
        limit: limit
      }
    ).fetchAsync();
  },

  /**
   * Gets repositories for a specific user.
   *
   * @method repositories.getUserRepos
   * @param {string} userId - The user ID to get repositories for.
   * @returns {Promise<Repository[]>} Array of user repositories.
   */
  async 'repositories.getUserRepos'(userId: string) {
    check(userId, String);

    return await RepositoriesCollection.find(
      { userID: userId },
      { sort: { createdAt: -1 } }
    ).fetchAsync();
  },

  /**
   * Gets repositories for the current user.
   *
   * @method repositories.getMyRepos
   * @returns {Promise<Repository[]>} Array of current user's repositories.
   */
  async 'repositories.getMyRepos'() {
    if (!this.userId) {
      throw new Meteor.Error('not-authorized', 'User must be logged in');
    }

    return await RepositoriesCollection.find(
      { userID: this.userId },
      { sort: { createdAt: -1 } }
    ).fetchAsync();
  },

  /**
   * Updates repository analysis status.
   *
   * @method repositories.updateStatus
   * @param {string} repoId - The repository ID.
   * @param {string} status - New status ('pending', 'completed', 'failed').
   * @param {object} metadata - Optional metadata to update.
   */
  async 'repositories.updateStatus'(repoId: string, status: string, metadata?: any) {
    check(repoId, String);
    check(status, String);

    if (!['pending', 'completed', 'failed'].includes(status)) {
      throw new Meteor.Error('invalid-status', 'Status must be pending, completed, or failed');
    }

    const updateData: any = {
      analysisStatus: status,
      updatedAt: new Date(),
      lastAnalyzed: new Date()
    };

    if (metadata) {
      updateData.metadata = metadata;
    }

    await RepositoriesCollection.updateAsync(
      { _id: repoId },
      { $set: updateData }
    );
  }
});
