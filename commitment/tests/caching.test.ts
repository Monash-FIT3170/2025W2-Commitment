import { Meteor } from "meteor/meteor";
import { expect } from "chai";

import {
  cacheIntoDatabase,
  isInDatabase,
  removeRepo,
  tryFromDatabaseNoCheck,
  allUrls,
  voidDatabase,
} from "../server/api/caching";

import { meteorCallAsync } from "../imports/api/meteor_interface";
import { RepositoryData } from "/imports/api/types";

const delay = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

describe("Caching Tests", () => {
  const testUrl = "https://github.com/test/repo";
  const testData = {
    name: "repo",
    branches: [],
    allCommits: new Map(),
    contributors: new Map(),
  } as RepositoryData;

  beforeEach(async () => {
    // Clean up before each test
    await voidDatabase();
  });

  afterEach(async () => {
    // Clean up after each test
    await voidDatabase();
  });

  it("should not find it", async () => {
    // looks for a url that isn't in the database
    expect(await isInDatabase(testUrl)).to.be.false;
  });

  it("should store and retrieve repository data", async () => {
    // Store data
    await cacheIntoDatabase(testUrl, testData);

    // Check if it exists
    expect(await isInDatabase(testUrl)).to.be.true;

    // Retrieve data
    expect(await tryFromDatabaseNoCheck(testUrl, null)).to.deep.equal(testData);
  });

  it("should return all URLs", async () => {
    // Store data
    await cacheIntoDatabase(testUrl, testData);

    expect(await allUrls()).to.include(testUrl);
  });

  it("should remove repository data", async () => {
    // Store data
    await cacheIntoDatabase(testUrl, testData);

    // Confirm it exists
    expect(await isInDatabase(testUrl)).to.be.true;

    // Remove data
    expect(await removeRepo(testUrl)).to.be.true;

    await delay(500); // simulates 500ms of work for the database to actually remove it

    // Check if it's gone
    expect(await isInDatabase(testUrl)).to.be.false;
  });

  it("should update last viewed timestamp", async () => {
    // Store data
    await cacheIntoDatabase(testUrl, testData);
    await meteorCallAsync("repoCollection.updateLastViewed")(testUrl);

    // This test just ensures the method doesn't throw an error
    expect(true).to.be.true;
  });
});
