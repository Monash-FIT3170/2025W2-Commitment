import { Meteor } from 'meteor/meteor'
import { expect } from 'chai'

import { cacheIntoDatabase, isInDatabase, tryFromDatabase } from "../server/caching"
import { meteorCallAsync, suppressError } from "../imports/api/meteor_interface"
import { RepositoryData } from '/imports/api/types'

describe('Caching Tests', () => {
  const testUrl = 'https://github.com/test/repo'
  const testData = { 
    name: 'test', 
    branches: [], 
    allCommits: new Map(), 
    contributors: new Map()
  } as RepositoryData

  beforeEach(async () => {
    // Clean up before each test
    await meteorCallAsync("repoCollection.removeRepo")(testUrl)
      .catch(suppressError)
  })

  afterEach(async () => {
    // Clean up after each test
    await meteorCallAsync("repoCollection.removeRepo")(testUrl)
      .catch(suppressError)
  })

  it('should store and retrieve repository data', async () => {
    // Store data
    await cacheIntoDatabase(testUrl, testData)

    // Check if it exists
    const exists = await isInDatabase(testUrl)
    expect(exists).to.be.true

    // Retrieve data
    const retrievedData = await tryFromDatabase(testUrl, null)
    expect(retrievedData).to.deep.equal(testData)
  })

  it('should return all URLs', async () => {
    // Store data
    await cacheIntoDatabase(testUrl, testData)
    
    const urls = await meteorCallAsync("repoCollection.allUrls")()
    expect(urls).to.include(testUrl)
  })

  it('should remove repository data', async () => {
    // Store data
    await cacheIntoDatabase(testUrl, testData)

    // Confirm it exists
    expect(await isInDatabase(testUrl)).to.be.true
    
    // Remove data
    await meteorCallAsync("repoCollection.removeRepo")(testUrl)
    
    // Check if it's gone
    expect(await isInDatabase(testUrl)).to.be.false
  })

  it('should update last viewed timestamp', async () => {
    // Store data
    await cacheIntoDatabase(testUrl, testData)
    await meteorCallAsync("repoCollection.updateLastViewed")(testUrl)
    
    // This test just ensures the method doesn't throw an error
    expect(true).to.be.true
  })
})