import { expect } from "chai";
import { Subject } from "rxjs";
import { RepoCollection, getRepoData } from "../server/caching";
import { RepositoryData } from "../server/commitment_api/types";
import sinon from "sinon";

describe("caching.ts", function () {
  const fakeRepo: RepositoryData = {
    name: "test-repo",
    branches: [],
    allCommits: {},
    contributors: {},
  };

  beforeEach(async function () {
    // Clear collection before each test
    await RepoCollection.removeAsync({});
    sinon.restore();
  });

  it("can insert and fetch repo data", async function () {
    await RepoCollection.insertAsync({ url: "http://test", data: fakeRepo });

    const found = await RepoCollection.findOneAsync({ url: "http://test" });
    expect(found?.data).to.deep.equal(fakeRepo);
  });

  it("returns data from database if present", async function () {
    await RepoCollection.insertAsync({
      url: "http://missing",
      data: fakeRepo,
    });
    const notifier = new Subject<string>();

    const result = await getRepoData("http://missing", notifier);
    expect(result.data).to.deep.equal(fakeRepo);
  });

  it("throws when removing missing repo", async function () {
    let error: unknown;
    try {
      await Meteor.callAsync(
        "repoCollection.removeRepo",
        "http://does-not-exist"
      );
    } catch (e) {
      error = e;
    }
    expect(error).to.exist;
    expect(error.error).to.equal("link-not-found");
  });
});
