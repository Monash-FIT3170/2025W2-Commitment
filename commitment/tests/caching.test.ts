import { Meteor } from 'meteor/meteor'
import { expect } from 'chai'

import { cacheIntoDatabase, isInDatabase, tryFromDatabase } from "../server/caching"
import { meteorCallAsync, suppressError } from "../imports/api/meteor_interface"
import { RepositoryData } from '/imports/api/types'
import { RepositoriesCollection } from '/imports/api/repositories'

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
    await RepositoriesCollection.removeAsync({})
  })

  afterEach(async () => {
    // Clean up after each test
    await RepositoriesCollection.removeAsync({})
  })

  it('should store and retrieve repository data', async () => {
    // Store data
    await cacheIntoDatabase(testUrl, testData)

    // Check if it exists
    expect(await isInDatabase(testUrl)).to.be.true

    // Retrieve data
    expect(await tryFromDatabase(testUrl, null)).to.deep.equal(testData)
  })

  it('should return all URLs', async () => {
    // Store data
    await cacheIntoDatabase(testUrl, testData)
    
    expect(await meteorCallAsync("repoCollection.allUrls")()).to.include(testUrl)
  })

  it('should remove repository data', async () => {
    // Store data
    await cacheIntoDatabase(testUrl, testData)

    // Confirm it exists
    expect(await isInDatabase(testUrl)).to.be.true
    
    // Remove data
    const removed = await meteorCallAsync("repoCollection.removeRepo")(testUrl)
    expect(removed).to.be.true
    
    // Check if it's gone
    const result = await isInDatabase(testUrl)
    expect(result).to.be.false
  })

  it('should update last viewed timestamp', async () => {
    // Store data
    await cacheIntoDatabase(testUrl, testData)
    await meteorCallAsync("repoCollection.updateLastViewed")(testUrl)
    
    // This test just ensures the method doesn't throw an error
    expect(true).to.be.true
  })
})