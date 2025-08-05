import { Meteor } from 'meteor/meteor';
import { getRepoData } from '/server/caching';
import { Subject } from 'rxjs';

Meteor.methods({
  async 'getGitHubRepoData'(url: string) {
    const notifier = new Subject<string>();
    try {
        const repoData = await getRepoData(url, notifier);
        return repoData; 
    } catch (error) {
        throw new Meteor.Error('repo-fetch-failed', `Failed to fetch repository data: ${error.message}`);
    }
  },
});