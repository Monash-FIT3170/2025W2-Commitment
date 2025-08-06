import { Meteor } from 'meteor/meteor';
import assert from 'assert';

const testUrl = async (url: string) =>
// in the frontend

  new Promise((resolve, reject) => {
    Meteor.call('getGitHubRepoData', url, (err: Error, result: boolean) => {
      if (err) reject(err);
      resolve(result);
    });
  });

describe('meteor-api-test-suite', () => {
  it('test 1: valid repo', async () => {Í
    const testRepo = 'https://github.com/Densetsu152637/test_repo_for_3170';
    assert.strictEqual(await testUrl(testRepo), true);
  });
  it('test 2: invalid repo', async () => {
    const testRepo = 'https://github.com/Densetsu152637/yeet_42069';
    assert.strictEqual(await testUrl(testRepo), true);
  });
});
