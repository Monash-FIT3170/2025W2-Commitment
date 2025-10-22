# Atlas Database Setup

## Table of Contents
- [Atlas Database Setup](#atlas-database-setup)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Creating an Atlas Account](#creating-an-atlas-account)
  - [Setting Up Your Cloud Database](#setting-up-your-cloud-database)

## Overview

To run this project, you will need to set up a MongoDB Atlas cloud database. Follow these steps to create your Atlas account and configure your database.

## Creating an Atlas Account

1. Navigate to [MongoDB Atlas](https://www.mongodb.com/atlas) in your web browser.
2. Click **"Try Free"** or **"Start Free"** to begin the registration process.
3. Fill out the registration form with your email address and create a secure password.
4. Verify your email address through the confirmation email sent by MongoDB.
5. Complete your profile setup and accept the terms of service.

## Setting Up Your Cloud Database

1. **Create a New Cluster**:
   - Once logged into Atlas, click **"Build a Database"** on the main dashboard.
   - Choose the **"FREE"** tier (M0) for development purposes.
   - Select a cloud provider and region closest to your location.
   - Give your cluster a name (e.g., "commitment-cluster") and click **"Create Cluster"**.

2. **Configure Database Access**:
   - Navigate to **"Database Access"** in the left sidebar.
   - Click **"Add New Database User"**.
   - Choose **"Password"** as the authentication method.
   - Create a username and generate a secure password (save these credentials securely).
   - Under **"Database User Privileges"**, select **"Atlas admin"** for full access.
   - Click **"Add User"** to create the database user.

3. **Configure Network Access**:
   - Go to **"Network Access"** in the left sidebar.
   - Click **"Add IP Address"**.
   - For development, you can click **"Allow Access from Anywhere"** (0.0.0.0/0) - this allows connections from any IP address.
   - Click **"Confirm"** to save the network access rule.

4. **Get Your Connection String**:
   - Return to **"Database"** in the left sidebar and click **"Connect"** on your cluster.
   - Choose **"Connect your application"**.
   - Select **"Node.js"** as the driver and version **"4.1 or later"**.
   - Copy the connection string that appears (it will look like: `mongodb+srv://<username>:<password>@cluster0.xxxxx.mongodb.net/?retryWrites=true&w=majority`).

5. **Update Your .env File**:
   - Replace the placeholder values in your `.env` file with your actual Atlas credentials:
     - The steps for this are down below