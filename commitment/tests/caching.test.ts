import { expect } from "chai";
import { Subject } from "rxjs";
import sinon from "sinon";

import { RepositoryData } from "../imports/api/types";
import { tryFromDatabase } from "/server/caching";

describe("caching.ts", () => {
  const fakeRepo: RepositoryData = {
    name: "test-repo",
    branches: [],
    allCommits: new Map(),
    contributors: new Map(),
  };

  beforeEach(async () => {
    // Clear collection before each test
    await Meteor.call("repoCollection.allUrls")
      .catch((_e: Error) => [])
      .then((urls: string[]) => 
          urls.forEach(async (url) => {
            await Meteor.call("repoCollection.removeRepo", url)
              .catch((_e: Error) => {})
          })
      );
    sinon.restore();
  });

  it("can insert and fetch repo data", async () => {
    const url = "http://test"
    await Meteor.call("repoCollection.insertOrUpdateRepoData", url, fakeRepo)
    const found = await Meteor.call("repoCollection.exists", url)
    expect(found).to.equal(true);
  });

  it("returns data from database if present", async function () {
    const url = "http://missing"
    await Meteor.call("repoCollection.insertOrUpdateRepoData", url, fakeRepo)
    const notifier = new Subject<string>();
    const result = await tryFromDatabase("http://missing", notifier)
      .catch((_e: Error) => expect(false).to.equal(true));
    expect(result.data).to.deep.equal(fakeRepo);
  });

  it("throws when removing missing repo", async function () {
    Meteor.callAsync(
        "repoCollection.removeRepo",
        "http://does-not-exist"
      )
      .then(_ => expect(false).to.equal(true))
      .catch((e: Meteor.Error) => expect(e.error).to.equal("not-in-database"))
  });
});
