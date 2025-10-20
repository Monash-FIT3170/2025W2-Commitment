import { expect } from "chai";

import { meteorCallAsync } from "@api/meteor_interface";
import { RepositoryData } from "@api/types";

import {
  cacheIntoDatabase,
  isInDatabase,
  removeRepo,
  tryFromDatabase,
  voidDatabase,
} from "../server/api/caching";

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
    expect(await tryFromDatabase(testUrl, null)).to.deep.equal(testData);
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
