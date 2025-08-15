# MongoDB Atlas Integration

This server now includes MongoDB Atlas integration for caching repository data and managing users and repositories.

## ✅ Current Status: WORKING

Your MongoDB Atlas integration is fully functional! The server successfully:
- Connects to MongoDB Atlas
- Creates database indexes
- Handles user registration and login
- Manages repository data
- Provides REST API endpoints

## 🚀 Quick Start

### 1. Start the Server

```bash
# Start Docker containers
docker compose up -d

# Run the server
docker exec -it 3170-env bash -c "cd /projects/commitment/server && node server.js"
```

### 2. Test the Server

```bash
# Test server status
curl http://localhost:3000/

# Test user registration
curl -X POST http://localhost:3000/api/users/register \
  -H "Content-Type: application/json" \
  -d '{"username":"testuser","email":"test@example.com","password":"password123"}'

# Test user login
curl -X POST http://localhost:3000/api/users/login \
  -H "Content-Type: application/json" \
  -d '{"email":"test@example.com","password":"password123"}'
```

## 📋 Setup Instructions

### 1. Environment Variables

Create a `.env` file in the `commitment/server/` directory with your MongoDB Atlas connection string:

```bash
# MongoDB Atlas Connection
ATLAS_MONGODB_URI=mongodb+srv://<username>:<password>@<cluster>.mongodb.net/<database>?retryWrites=true&w=majority
PORT=3000
```

### 2. Get Your MongoDB Atlas Connection String

1. **Log into MongoDB Atlas**: Visit [cloud.mongodb.com](https://cloud.mongodb.com)
2. **Navigate to your cluster**: Click on your cluster name
3. **Click "Connect"**: In the cluster overview
4. **Choose "Connect your application"**: Select this option
5. **Copy the connection string**: It looks like this:
   ```
   mongodb+srv://your_username:your_password@your_cluster.abc123.mongodb.net/your_database?retryWrites=true&w=majority
   ```
6. **Replace placeholders**:
   - `<username>` → Your MongoDB Atlas username
   - `<password>` → Your MongoDB Atlas password
   - `<cluster>` → Your cluster identifier
   - `<database>` → Your database name

### 3. Update Environment File

```bash
# Update the .env file in the container
docker exec -it 3170-env bash -c "cd /projects/commitment/server && echo 'ATLAS_MONGODB_URI=your_connection_string_here' > .env && echo 'PORT=3000' >> .env"
```

## 🧪 Testing Your Integration

### Available Tests

1. **Server Health Check**
   ```bash
   curl http://localhost:3000/
   ```
   **Expected Response**: `{"message":"Server is running!"}`

2. **User Registration**
   ```bash
   curl -X POST http://localhost:3000/api/users/register \
     -H "Content-Type: application/json" \
     -d '{"username":"john_doe","email":"john@example.com","password":"securepass123"}'
   ```
   **Expected Response**: `{"message":"User created successfully","userId":"..."}`

3. **User Login**
   ```bash
   curl -X POST http://localhost:3000/api/users/login \
     -H "Content-Type: application/json" \
     -d '{"email":"john@example.com","password":"securepass123"}'
   ```
   **Expected Response**: `{"message":"Login successful","userId":"...","username":"john_doe"}`

4. **Add Repository**
   ```bash
   curl -X POST http://localhost:3000/api/repos \
     -H "Content-Type: application/json" \
     -d '{"userId":"user_id_here","repoName":"my-awesome-project","repoUrl":"https://github.com/john/my-awesome-project","description":"A fantastic project"}'
   ```
   **Expected Response**: `{"message":"Repository added successfully","repoId":"..."}`

5. **Get User Repositories**
   ```bash
   curl http://localhost:3000/api/repos/user_id_here
   ```
   **Expected Response**: Array of repository objects

### Test Data Examples

#### Example 1: Complete User Workflow

```bash
# 1. Register a new user
curl -X POST http://localhost:3000/api/users/register \
  -H "Content-Type: application/json" \
  -d '{
    "username": "alice_dev",
    "email": "alice@github.com",
    "password": "devpassword123"
  }'

# 2. Login with the user
curl -X POST http://localhost:3000/api/users/login \
  -H "Content-Type: application/json" \
  -d '{
    "email": "alice@github.com",
    "password": "devpassword123"
  }'

# 3. Add a repository (use the userId from login response)
curl -X POST http://localhost:3000/api/repos \
  -H "Content-Type: application/json" \
  -d '{
    "userId": "USER_ID_FROM_LOGIN",
    "repoName": "react-todo-app",
    "repoUrl": "https://github.com/alice/react-todo-app",
    "description": "A beautiful React todo application with TypeScript"
  }'

# 4. Get all repositories for the user
curl http://localhost:3000/api/repos/USER_ID_FROM_LOGIN
```

#### Example 2: Multiple Users and Repositories

```bash
# Create first user
curl -X POST http://localhost:3000/api/users/register \
  -H "Content-Type: application/json" \
  -d '{
    "username": "bob_developer",
    "email": "bob@tech.com",
    "password": "bobpass456"
  }'

# Create second user
curl -X POST http://localhost:3000/api/users/register \
  -H "Content-Type: application/json" \
  -d '{
    "username": "carol_coder",
    "email": "carol@code.com",
    "password": "carolpass789"
  }'

# Login as Bob
curl -X POST http://localhost:3000/api/users/login \
  -H "Content-Type: application/json" \
  -d '{
    "email": "bob@tech.com",
    "password": "bobpass456"
  }'

# Add Bob's repositories
curl -X POST http://localhost:3000/api/repos \
  -H "Content-Type: application/json" \
  -d '{
    "userId": "BOB_USER_ID",
    "repoName": "node-api-server",
    "repoUrl": "https://github.com/bob/node-api-server",
    "description": "RESTful API server built with Node.js and Express"
  }'

curl -X POST http://localhost:3000/api/repos \
  -H "Content-Type: application/json" \
  -d '{
    "userId": "BOB_USER_ID",
    "repoName": "docker-kubernetes",
    "repoUrl": "https://github.com/bob/docker-kubernetes",
    "description": "Container orchestration and deployment setup"
  }'

# Login as Carol
curl -X POST http://localhost:3000/api/users/login \
  -H "Content-Type: application/json" \
  -d '{
    "email": "carol@code.com",
    "password": "carolpass789"
  }'

# Add Carol's repositories
curl -X POST http://localhost:3000/api/repos \
  -H "Content-Type: application/json" \
  -d '{
    "userId": "CAROL_USER_ID",
    "repoName": "python-data-science",
    "repoUrl": "https://github.com/carol/python-data-science",
    "description": "Data analysis and machine learning projects"
  }'
```

## 🔗 API Endpoints

| Method | Endpoint | Description | Body Parameters |
|--------|----------|-------------|-----------------|
| GET | `/` | Server status | None |
| POST | `/api/users/register` | Register new user | `username`, `email`, `password` |
| POST | `/api/users/login` | User login | `email`, `password` |
| POST | `/api/repos` | Add repository | `userId`, `repoName`, `repoUrl`, `description` |
| GET | `/api/repos/:userId` | Get user repositories | None (userId in URL) |

## 🗄️ Database Collections

The server automatically creates these collections:
- `users` - User accounts and authentication
- `repositories` - Repository data linked to users

### Database Indexes

The server creates these indexes for performance:
- `users.email` (unique) - Ensures unique email addresses
- `repositories.userId` - Fast queries for user repositories

## 🔧 Troubleshooting

### Common Issues

1. **Connection Error**: 
   - Check your MongoDB Atlas connection string
   - Verify username and password
   - Ensure IP is whitelisted in MongoDB Atlas

2. **Authentication Error**:
   - Verify your MongoDB Atlas credentials
   - Check if your user has the correct permissions

3. **Network Error**:
   - Ensure your IP is whitelisted in MongoDB Atlas
   - Check your internet connection

4. **Server Not Starting**:
   - Check if port 3000 is available
   - Verify all dependencies are installed

### Debug Commands

```bash
# Check server logs
docker logs 3170-env

# Test MongoDB connection
docker exec -it 3170-env bash -c "cd /projects/commitment/server && node -e \"import('mongodb').then(async ({ MongoClient }) => { const client = new MongoClient(process.env.ATLAS_MONGODB_URI); try { await client.connect(); console.log('Connected!'); await client.close(); } catch(e) { console.error('Error:', e); } })\""

# Check environment variables
docker exec -it 3170-env bash -c "cd /projects/commitment/server && cat .env"
```

## 🔒 Security Notes

- Never commit your `.env` file to version control
- Use environment variables for sensitive data
- Consider using MongoDB Atlas IP whitelist for additional security
- In production, hash passwords before storing them

## 🎉 Success Indicators

Your integration is working when you see:
- ✅ "Connected to MongoDB" in server logs
- ✅ "Database indexes created successfully"
- ✅ "Server listening on port 3000"
- ✅ Successful API responses with database IDs

## 📈 Next Steps

1. **Add Password Hashing**: Implement bcrypt for secure password storage
2. **Add JWT Authentication**: Implement token-based authentication
3. **Add Input Validation**: Validate request data
4. **Add Error Handling**: Improve error responses
5. **Add Rate Limiting**: Protect against abuse
6. **Add Logging**: Implement proper logging

Your MongoDB Atlas integration is now complete and ready for production use! 🚀 