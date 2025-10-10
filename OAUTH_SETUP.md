# OAuth Authentication Setup Guide

## Overview

This application supports third-party authentication via Google and GitHub OAuth providers. Users can sign up or log in using their Google or GitHub accounts in addition to traditional email/password authentication.

## Critical Information: Will the App Work Without OAuth?

**YES - The application will continue to work without OAuth credentials configured.**

- ✅ **Email/password authentication** will work normally
- ✅ Users can sign up and log in with username/email/password
- ⚠️ **OAuth login buttons** will be visible but will fail with error messages if clicked
- ⚠️ Users attempting to use Google/GitHub login will see error alerts

**Bottom Line:** OAuth is an **optional enhancement** that improves user experience but is not required for core functionality.

---

## Table of Contents

1. [Development Setup](#development-setup)
2. [Production Setup](#production-setup)
3. [Troubleshooting](#troubleshooting)
4. [Security Notes](#security-notes)

---

## Development Setup

Follow these steps to enable Google and GitHub OAuth for local development.

### Prerequisites

- Local development environment running on `http://localhost:3000`
- Access to Google Cloud Console (for Google OAuth)
- Access to GitHub account settings (for GitHub OAuth)

### Step 1: Create Google OAuth App

1. Go to [Google Cloud Console](https://console.cloud.google.com/)
2. Create a new project or select an existing one
3. Navigate to **APIs & Services** → **Credentials**
4. Click **Create Credentials** → **OAuth client ID**
5. Configure the OAuth consent screen if prompted:
   - User Type: **External** (for testing) or **Internal** (for organization use)
   - App name: `Commitment` (or your preferred name)
   - User support email: Your email
   - Developer contact: Your email
6. Create OAuth client ID:
   - Application type: **Web application**
   - Name: `Commitment Dev`
   - Authorized JavaScript origins: `http://localhost:3000`
   - Authorized redirect URIs: `http://localhost:3000/_oauth/google`
7. Click **Create**
8. Copy the **Client ID** and **Client Secret** (you'll need these for the `.env` file)

### Step 2: Create GitHub OAuth App

1. Go to [GitHub Settings](https://github.com/settings/developers)
2. Click **OAuth Apps** → **New OAuth App**
3. Fill in the application details:
   - Application name: `Commitment Dev`
   - Homepage URL: `http://localhost:3000`
   - Authorization callback URL: `http://localhost:3000/_oauth/github`
4. Click **Register application**
5. Copy the **Client ID**
6. Click **Generate a new client secret**
7. Copy the **Client Secret** (you won't be able to see it again)

### Step 3: Configure Environment Variables

1. Navigate to the `commitment` directory:
   ```bash
   cd commitment
   ```

2. Open the `.env` file (create it if it doesn't exist)

3. Add the following OAuth credentials:
   ```bash
   # Existing variables (keep these)
   MONGO_URL=mongodb+srv://...
   SERVER_HOST=0.0.0.0
   PORT=3000
   DB_NAME=commitment_db
   NODE_ENV=development

   # OAuth Configuration (add these)
   GOOGLE_CLIENT_ID=your_google_client_id_here
   GOOGLE_CLIENT_SECRET=your_google_client_secret_here
   GITHUB_CLIENT_ID=your_github_client_id_here
   GITHUB_CLIENT_SECRET=your_github_client_secret_here
   ```

4. Replace the placeholder values with your actual credentials

### Step 4: Restart the Application

```bash
# Stop the current Meteor server (Ctrl+C if running)
# Then restart it
npm start
# or
meteor
```

The OAuth configuration will be loaded on server startup.

### Step 5: Test OAuth Login

1. Navigate to `http://localhost:3000/login` or `http://localhost:3000/signup`
2. You should see **Log in with Google** and **Log in with GitHub** buttons
3. Click either button to test the OAuth flow
4. You should be redirected to Google/GitHub for authentication
5. After approving, you'll be redirected back and logged into the application

---

## Production Setup

Follow these steps to enable OAuth for the deployed production environment on EC2.

### Prerequisites

- SSH access to the EC2 production server
- Production domain or IP address (e.g., `http://your-ec2-ip:3000` or `https://your-domain.com`)

### Step 1: Create Google OAuth App for Production

1. Go to [Google Cloud Console](https://console.cloud.google.com/)
2. Navigate to your project → **APIs & Services** → **Credentials**
3. Either create a new OAuth client ID or add redirect URIs to the existing one:
   - Click **Create Credentials** → **OAuth client ID** (or edit existing)
   - Application type: **Web application**
   - Name: `Commitment Production`
   - Authorized JavaScript origins:
     - `http://YOUR_EC2_IP:3000` (replace with actual IP)
     - `https://your-domain.com` (if using a domain)
   - Authorized redirect URIs:
     - `http://YOUR_EC2_IP:3000/_oauth/google`
     - `https://your-domain.com/_oauth/google` (if using a domain)
4. Click **Create** or **Save**
5. Copy the **Client ID** and **Client Secret**

### Step 2: Create GitHub OAuth App for Production

1. Go to [GitHub Settings](https://github.com/settings/developers)
2. Click **OAuth Apps** → **New OAuth App**
3. Fill in the application details:
   - Application name: `Commitment Production`
   - Homepage URL: `http://YOUR_EC2_IP:3000` (or your domain)
   - Authorization callback URL:
     - `http://YOUR_EC2_IP:3000/_oauth/github`
     - Or `https://your-domain.com/_oauth/github` (if using a domain)
4. Click **Register application**
5. Copy the **Client ID**
6. Generate and copy the **Client Secret**

### Step 3: Configure Production Environment Variables

1. SSH into your EC2 instance:
   ```bash
   ssh -i your-key.pem ubuntu@YOUR_EC2_IP
   ```

2. Navigate to the application directory:
   ```bash
   cd ~/app/commitment
   ```

3. Edit the `.env` file:
   ```bash
   nano .env
   # or
   vim .env
   ```

4. Add the OAuth credentials:
   ```bash
   # Existing variables (keep these)
   MONGO_URL=mongodb+srv://...
   SERVER_HOST=0.0.0.0
   PORT=3000
   DB_NAME=commitment_db
   NODE_ENV=production

   # OAuth Configuration (add these)
   GOOGLE_CLIENT_ID=your_production_google_client_id
   GOOGLE_CLIENT_SECRET=your_production_google_client_secret
   GITHUB_CLIENT_ID=your_production_github_client_id
   GITHUB_CLIENT_SECRET=your_production_github_client_secret
   ```

5. Save and exit the file (in nano: `Ctrl+X`, `Y`, `Enter`)

### Step 4: Restart the Production Containers

```bash
cd ~/app
sudo docker-compose -f docker-compose.prod.yml down
sudo docker-compose -f docker-compose.prod.yml up -d
```

### Step 5: Verify Production OAuth

1. Navigate to your production URL: `http://YOUR_EC2_IP:3000/login`
2. Test the Google and GitHub login buttons
3. Verify successful authentication and redirect

---

## Troubleshooting

### "OAuth login failed" Error

**Possible Causes:**
- OAuth credentials not configured in `.env`
- Incorrect Client ID or Client Secret
- Server not restarted after adding credentials

**Solution:**
1. Verify credentials in `.env` file match those from Google/GitHub
2. Ensure no extra spaces or quotes around the values
3. Restart the server/containers

### Redirect URI Mismatch Error

**Symptom:** Error message from Google/GitHub about redirect URI mismatch

**Solution:**
1. Verify the redirect URI in Google/GitHub app settings exactly matches:
   - Development: `http://localhost:3000/_oauth/google` (or `github`)
   - Production: `http://YOUR_ACTUAL_URL/_oauth/google` (or `github`)
2. Ensure the URL includes the correct protocol (`http` vs `https`)
3. Check for trailing slashes (should NOT have one)

### OAuth Popup Blocked

**Symptom:** Nothing happens when clicking OAuth button

**Solution:**
- Enable popups for your domain in browser settings
- The OAuth flow uses `loginStyle: 'popup'` by default

### Environment Variables Not Loading

**Solution:**
1. Verify `.env` file is in the `commitment` directory (not the root)
2. Check `docker-compose.prod.yml` includes `env_file: - ./commitment/.env`
3. Restart containers to pick up new environment variables

---

## Security Notes

### ⚠️ Important Security Practices

1. **Never commit `.env` to Git**
   - The `.env` file is already in `.gitignore`
   - Double-check before committing any changes

2. **Use different credentials for dev and production**
   - Create separate OAuth apps for development and production
   - This allows you to revoke/rotate credentials independently

3. **Rotate secrets regularly**
   - Periodically regenerate OAuth client secrets
   - Update both the OAuth provider and `.env` file

4. **Limit OAuth app permissions**
   - Only request the minimum necessary scopes
   - Current implementation uses default scopes (basic profile info)

5. **Use HTTPS in production**
   - Configure SSL/TLS certificates for your production domain
   - Update OAuth redirect URIs to use `https://`

6. **Secure the EC2 instance**
   - Restrict `.env` file permissions: `chmod 600 commitment/.env`
   - Ensure only authorized users have SSH access

---

## Additional Resources

- [Meteor OAuth Documentation](https://docs.meteor.com/api/accounts.html#OAuth)
- [Google OAuth 2.0 Documentation](https://developers.google.com/identity/protocols/oauth2)
- [GitHub OAuth Documentation](https://docs.github.com/en/apps/oauth-apps/building-oauth-apps/authorizing-oauth-apps)

---

## Support

If you encounter issues not covered in this guide:

1. Check the server logs for error messages
2. Verify OAuth configuration in `commitment/server/oauth-config.ts`
3. Ensure the import exists in `commitment/server/main.ts`
4. Contact the development team for assistance
