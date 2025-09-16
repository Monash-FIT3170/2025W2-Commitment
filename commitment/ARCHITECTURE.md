# 🗄️ Database Architecture & Data Storage Guide

## 📋 Overview

This document explains how data is stored, accessed, and managed in the Commitment project. Our application uses a **hybrid architecture** with both Meteor (frontend) and Express.js (backend) servers connecting to MongoDB Atlas.

## 🏗️ Architecture Overview

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Meteor App    │    │  Express API    │    │  MongoDB Atlas  │
│   (Frontend)    │◄──►│   (Backend)     │◄──►│   (Database)    │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## 🗃️ Database Collections

### 1. **Links Collection** (`links`)
**Purpose**: Basic link storage for the application
**Schema**:
```typescript
interface Link {
  _id?: string;        // MongoDB ObjectId
  title: string;       // Display name for the link
  url: string;         // Full URL (must start with http/https)
  createdAt: Date;     // When the link was created
}
```

**Usage**: General link management, not user-specific

### 2. **Bookmarks Collection** (`bookmarks`)
**Purpose**: User-specific bookmarks with tracking
**Schema**:
```typescript
interface Bookmark {
  _id?: string;        // MongoDB ObjectId
  title: string;       // Display name for the bookmark
  url: string;         // Full URL (must start with http/https)
  createdAt: Date;     // When the bookmark was created
  userID: string;      // Meteor user ID who owns this bookmark
  lastViewed?: Date;   // When the user last viewed this bookmark
}
```

**Usage**: Personal bookmark management for authenticated users

### 3. **Repositories Collection** (`repositories`)
**Purpose**: Cached GitHub repository data from Express server
**Schema**:
```typescript
interface Repository {
  _id?: string;        // MongoDB ObjectId
  url: string;         // GitHub repository URL
  name: string;        // Repository name
  branches: string[];  // Available branches
  allCommits: object;  // Commit data by branch
  contributors: object; // Contributor information
  createdAt: Date;     // When first cached
  updatedAt: Date;     // When last updated
}
```

**Usage**: Caching repository data to avoid repeated GitHub API calls

### 4. **Server Messages Collection** (`fetchRepoMessagesCollection`)
**Purpose**: Communication between frontend and backend
**Schema**:
```typescript
interface ServerMessage {
  _id?: string;        // MongoDB ObjectId
  text: string;        // Message content
  createdAt: Date;     // When message was created
}
```

**Usage**: Real-time updates and status messages

## 🔐 Data Access Methods

### Frontend (Meteor) Access

#### Links Management
```typescript
// Insert a new link
Meteor.call('links.insert', title, url, (err, result) => {
  if (err) console.error('Error:', err);
  else console.log('Link ID:', result);
});

// Remove a link
Meteor.call('links.remove', url, (err, result) => {
  if (err) console.error('Error:', err);
  else console.log('Removed:', result);
});

// Check if link exists
Meteor.call('links.isBookmarked', url, (err, result) => {
  if (err) console.error('Error:', err);
  else console.log('Exists:', result);
});
```

#### Bookmarks Management
```typescript
// Add a bookmark (requires login)
Meteor.call('bookmarks.insertBookmark', title, url, (err, result) => {
  if (err) console.error('Error:', err);
  else console.log('Bookmark ID:', result);
});

// Remove a bookmark
Meteor.call('bookmarks.removeBookmark', url, (err, result) => {
  if (err) console.error('Error:', err);
  else console.log('Removed:', result);
});

// Get all user bookmarks
Meteor.call('bookmarks.getAllBookmarks', (err, result) => {
  if (err) console.error('Error:', err);
  else console.log('Bookmarks:', result);
});

// Get N most recent bookmarks
Meteor.call('bookmarks.getNBookmarks', 5, (err, result) => {
  if (err) console.error('Error:', err);
  else console.log('Recent bookmarks:', result);
});

// Update last viewed time
Meteor.call('bookmarks.updateLastViewed', url, (err, result) => {
  if (err) console.error('Error:', err);
  else console.log('Updated:', result);
});
```

#### Repository Data Access
```typescript
import { fetchRepo, repoInDatabase } from '/imports/api/call_repo';

// Check if repository exists in database
const exists = await repoInDatabase(githubUrl);
console.log('Repository exists:', exists);

// Fetch repository data (triggers backend processing)
const success = await fetchRepo(githubUrl, messageSubject);
console.log('Fetch successful:', success);
```

### Backend (Express) Access

#### Direct Database Access
```typescript
import { getDatabase, getRepositoryData, cacheRepositoryData } from './services/atlas';

// Get database instance
const db = await getDatabase();

// Access collections directly
const linksCollection = db.collection('links');
const bookmarksCollection = db.collection('bookmarks');
const repositoriesCollection = db.collection('repositories');

// Query examples
const allLinks = await linksCollection.find({}).toArray();
const userBookmarks = await bookmarksCollection.find({ userID: 'userId' }).toArray();
const cachedRepo = await repositoriesCollection.findOne({ url: 'githubUrl' });
```

#### API Endpoints
```bash
# Health check
GET /health

# List all collections
GET /atlas/list

# Get repository statistics
GET /stats

# Check if repository is cached
GET /repo/:url

# Database operations (if you add them)
POST /repo/cache
DELETE /repo/:url
```

## 🚀 Data Flow

### 1. **Adding a New Repository**
```
User Input → Meteor Frontend → Express Backend → GitHub API → MongoDB Cache
     ↓              ↓              ↓              ↓           ↓
  Repository URL → Validation → Data Fetch → Process → Store
```

### 2. **Bookmarking a Repository**
```
User Action → Check if repo exists → Validate URL → Create bookmark → Store in DB
     ↓              ↓                    ↓           ↓              ↓
  Click Bookmark → repoInDatabase() → Validation → Insert → Success
```

### 3. **Data Retrieval**
```
User Request → Check cache → Return cached data OR Fetch fresh data
     ↓              ↓              ↓
  View Repo → MongoDB Lookup → Display or Update
```

## 🔧 Development Setup

### Prerequisites
- MongoDB Atlas account
- Node.js 18+
- Meteor (for frontend development)

### Environment Variables
```bash
# MongoDB Connection
MONGODB_URI=mongodb+srv://username:password@cluster.mongodb.net/database
DB_NAME=commitment_db

# Server Configuration
SERVER_PORT=3000
NODE_ENV=development
```

### Local Development
```bash
# Start Express backend
cd commitment/server
npm install
npm start

# Start Meteor frontend (in another terminal)
cd commitment
meteor
```

## 📊 Data Validation Rules

### URL Validation
- ✅ Must start with `http://` or `https://`
- ✅ Must be a valid URL format
- ❌ Relative URLs are not allowed

### Authentication Requirements
- **Links**: No authentication required
- **Bookmarks**: User must be logged in
- **Repository data**: No authentication required

### Data Integrity
- **Duplicate URLs**: Allowed in links, but not in user bookmarks
- **Required Fields**: title, url, createdAt
- **Optional Fields**: lastViewed (bookmarks only)

## 🚨 Common Issues & Solutions

### 1. **"URL does not exist in database" Error**
**Cause**: Repository hasn't been fetched yet
**Solution**: Use `fetchRepo()` first, then bookmark

### 2. **"Server is not connected" Error**
**Cause**: Meteor client not connected to server
**Solution**: Check internet connection and server status

### 3. **"Not authorized" Error**
**Cause**: User not logged in
**Solution**: Implement user authentication

### 4. **Database Connection Issues**
**Cause**: MongoDB Atlas connection problems
**Solution**: Check MONGODB_URI and network connectivity

## 📈 Performance Considerations

### Caching Strategy
- Repository data is cached to avoid repeated GitHub API calls
- Cache expires after 24 hours (configurable)
- Bookmarks are always fresh (no caching)

### Database Indexes
Consider adding indexes for:
```javascript
// Links collection
db.links.createIndex({ "url": 1 });

// Bookmarks collection
db.bookmarks.createIndex({ "userID": 1, "url": 1 });
db.bookmarks.createIndex({ "lastViewed": -1 });

// Repositories collection
db.repositories.createIndex({ "url": 1 });
db.repositories.createIndex({ "updatedAt": -1 });
```

## 🔍 Monitoring & Debugging

### Logs to Watch
- Express server logs: `console.log` in server.js
- Meteor client logs: Browser console
- Database operations: MongoDB Atlas dashboard

### Useful Queries
```javascript
// Check recent activity
db.repositories.find({}, {sort: {updatedAt: -1}, limit: 10});

// User bookmark count
db.bookmarks.countDocuments({userID: "userId"});

// Cache hit rate
db.repositories.countDocuments({updatedAt: {$gte: new Date(Date.now() - 24*60*60*1000)}});
```

## 📞 Team Communication

### When Making Changes
1. **Update this document** if you modify the data structure
2. **Test thoroughly** before deploying
3. **Coordinate with team** if changing shared collections
4. **Document new endpoints** or methods

### Questions & Support
- Check this document first
- Review the code examples above
- Ask the team lead for database schema changes
- Test in development environment first

---

**Last Updated**: [Current Date]
**Maintained By**: [Your Name/Team]
**Version**: 1.0
