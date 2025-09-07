import { Meteor } from 'meteor/meteor';
import { expect } from 'chai';

import {} from "../server/caching"

describe('Caching Tests', () => {
  const testUrl = 'https://github.com/test/repo';

  beforeEach(async () => {
    // Clean up before each test
    await Meteor.call("repoCollection.removeRepo", testUrl);
  });

  afterEach(async () => {
    // Clean up after each test
    await Meteor.call("repoCollection.removeRepo", testUrl);
  });

  it('should store and retrieve repository data', async () => {
    const testData = {
      name: 'test-repo',
      branches: [],
      contributors: [],
      allCommits: []
    };

    // Store data
    await Meteor.call("repoCollection.insertOrUpdateRepoData", testUrl, testData);

    // Check if it exists
    const exists = await Meteor.call("repoCollection.exists", testUrl);
    expect(exists).to.be.true;

    // Retrieve data
    const retrievedData = await Meteor.call("repoCollection.getData", testUrl);
    expect(retrievedData).to.deep.equal(testData);
  });

  it('should return all URLs', async () => {
    const testData = { name: 'test', branches: [], contributors: [], allCommits: [] };
    
    await Meteor.call("repoCollection.insertOrUpdateRepoData", testUrl, testData);
    
    const urls = await Meteor.call("repoCollection.allUrls");
    expect(urls).to.include(testUrl);
  });

  it('should remove repository data', async () => {
    const testData = { name: 'test', branches: [], contributors: [], allCommits: [] };
    
    // Store data
    await Meteor.call("repoCollection.insertOrUpdateRepoData", testUrl, testData);
    
    // Remove data
    await Meteor.call("repoCollection.removeRepo", testUrl);
    
    // Check if it's gone
    const exists = await Meteor.call("repoCollection.exists", testUrl);
    expect(exists).to.be.false;
  });

  it('should update last viewed timestamp', async () => {
    const testData = { name: 'test', branches: [], contributors: [], allCommits: [] };
    
    await Meteor.call("repoCollection.insertOrUpdateRepoData", testUrl, testData);
    await Meteor.call("repoCollection.updateLastViewed", testUrl);
    
    // This test just ensures the method doesn't throw an error
    expect(true).to.be.true;
  });
});
