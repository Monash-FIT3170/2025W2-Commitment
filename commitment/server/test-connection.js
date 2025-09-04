import dotenv from 'dotenv';
import { connectToDatabase, listCollections, getRepositoryStats } from './src/services/atlas.js';

// Load environment variables
dotenv.config();

async function testConnection() {
  console.log('🔍 Testing MongoDB Atlas connection...');
  
  try {
    // Test database connection
    await connectToDatabase();
    console.log('✅ Database connection successful!');
    
    // Test listing collections
    console.log('\n📋 Testing collection listing...');
    const collections = await listCollections();
    console.log('✅ Collections listed successfully:', collections.map(c => c.name));
    
    // Test getting stats
    console.log('\n📊 Testing repository statistics...');
    const stats = await getRepositoryStats();
    console.log('✅ Repository stats retrieved:', stats);
    
    console.log('\n🎉 All tests passed! Your MongoDB Atlas connection is working correctly.');
    
  } catch (error) {
    console.error('❌ Connection test failed:', error.message);
    console.log('\n💡 Troubleshooting tips:');
    console.log('1. Check your MONGODB_URI in the .env file');
    console.log('2. Verify your MongoDB Atlas credentials');
    console.log('3. Ensure your IP is whitelisted in MongoDB Atlas');
    console.log('4. Check your internet connection');
  }
}

// Run the test
testConnection(); 