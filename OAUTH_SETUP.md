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

## 🔑 Important: Using Repository Secrets for Development

**For development team members:** Instead of creating your own OAuth applications, use the shared development OAuth credentials stored in the GitHub repository secrets. This:

- ✅ Simplifies onboarding for new developers
- ✅ Prevents proliferation of OAuth applications
- ✅ Ensures consistent configuration across the team
- ✅ Centralizes credential management

**Only create new OAuth apps if:**
- You're deploying to production
- You don't have access to the repository secrets
- You need to test with custom OAuth configurations

---

## Table of Contents

1. [Development Setup (Using Repository Secrets)](#development-setup-using-repository-secrets)
2. [Development Setup (Creating Your Own OAuth Apps)](#development-setup-creating-your-own-oauth-apps)
3. [Production OAuth](#production-oauth)
4. [Troubleshooting](#troubleshooting)
5. [Security Notes](#security-notes)

---

## Development Setup (Using Repository Secrets)

**Recommended for development team members.**

### Prerequisites

- Access to the GitHub repository
- Local development environment running on `http://localhost:3000`

### Step 1: Access Repository Secrets

1. Navigate to the GitHub repository page
2. Go to **Settings** → **Secrets and variables** → **Actions** (or **Codespaces** depending on setup)
3. Locate the development OAuth secrets:
   - `DEV_GOOGLE_CLIENT_ID`
   - `DEV_GOOGLE_CLIENT_SECRET`
   - `DEV_GITHUB_CLIENT_ID`
   - `DEV_GITHUB_CLIENT_SECRET`
4. Copy these values (you may need repository admin access to view secrets)

> **Note:** If you don't have access to view secrets, contact your team lead or repository administrator.

### Step 2: Configure Environment Variables

1. Navigate to the `commitment` directory:
   ```bash
   cd commitment
   ```

2. Open the `.env` file (create it if it doesn't exist)

3. Add the OAuth credentials from repository secrets:
   ```bash
   # Existing variables (keep these)
   MONGO_URL=mongodb+srv://...
   SERVER_HOST=0.0.0.0
   PORT=3000
   DB_NAME=commitment_db
   NODE_ENV=development

   # OAuth Configuration (paste from repository secrets)
   GOOGLE_CLIENT_ID=<value_from_DEV_GOOGLE_CLIENT_ID>
   GOOGLE_CLIENT_SECRET=<value_from_DEV_GOOGLE_CLIENT_SECRET>
   GITHUB_CLIENT_ID=<value_from_DEV_GITHUB_CLIENT_ID>
   GITHUB_CLIENT_SECRET=<value_from_DEV_GITHUB_CLIENT_SECRET>
   ```

4. Replace the `<value_from_...>` placeholders with the actual values from GitHub repository secrets

### Step 3: Restart the Application

```bash
# Stop the current Meteor server (Ctrl+C if running)
# Then restart it
npm start
# or
meteor
```

The OAuth configuration will be loaded on server startup.

### Step 4: Test OAuth Login

1. Navigate to `http://localhost:3000/login` or `http://localhost:3000/signup`
2. You should see **Log in with Google** and **Log in with GitHub** buttons
3. Click either button to test the OAuth flow
4. You should be redirected to Google/GitHub for authentication
5. After approving, you'll be redirected back and logged into the application

---

## Development Setup (Creating Your Own OAuth Apps)

**Only use this approach if you cannot access repository secrets or need custom OAuth configuration for testing.**

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
   - App name: `Commitment Dev - Your Name` (use your name to distinguish it)
   - User support email: Your email
   - Developer contact: Your email
6. Create OAuth client ID:
   - Application type: **Web application**
   - Name: `Commitment Dev - Your Name`
   - Authorized JavaScript origins: `http://localhost:3000`
   - Authorized redirect URIs: `http://localhost:3000/_oauth/google`
7. Click **Create**
8. Copy the **Client ID** and **Client Secret**

### Step 2: Create GitHub OAuth App

1. Go to [GitHub Settings](https://github.com/settings/developers)
2. Click **OAuth Apps** → **New OAuth App**
3. Fill in the application details:
   - Application name: `Commitment Dev - Your Name`
   - Homepage URL: `http://localhost:3000`
   - Authorization callback URL: `http://localhost:3000/_oauth/github`
4. Click **Register application**
5. Copy the **Client ID**
6. Click **Generate a new client secret**
7. Copy the **Client Secret**

### Step 3: Configure Environment Variables

1. Navigate to the `commitment` directory:
   ```bash
   cd commitment
   ```

2. Open the `.env` file (create it if it doesn't exist)

3. Add your OAuth credentials:
   ```bash
   # Existing variables (keep these)
   MONGO_URL=mongodb+srv://...
   SERVER_HOST=0.0.0.0
   PORT=3000
   DB_NAME=commitment_db
   NODE_ENV=development

   # OAuth Configuration (your personal dev credentials)
   GOOGLE_CLIENT_ID=your_google_client_id_here
   GOOGLE_CLIENT_SECRET=your_google_client_secret_here
   GITHUB_CLIENT_ID=your_github_client_id_here
   GITHUB_CLIENT_SECRET=your_github_client_secret_here
   ```

### Step 4: Restart and Test

Follow steps 3-4 from the "Development Setup (Using Repository Secrets)" section above.

---

## Production OAuth

**Production OAuth credentials are managed by the deployment team only.**

For general developers: You do not need to configure or access production OAuth credentials. The production environment uses separate OAuth applications that are managed and deployed by the deployment team.

**If you are part of the deployment team:**
- Production OAuth credentials are stored securely (not in repository secrets)
- Contact the deployment team lead for access to production credentials
- Production OAuth apps should use HTTPS and the production domain/IP
- Follow your organization's production deployment and security procedures

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
   - Never share secrets in pull requests, issues, or documentation

2. **Repository Secrets Management**
   - Only repository administrators should have access to create/modify secrets
   - Development OAuth secrets should be stored in GitHub repository secrets
   - Use the naming convention: `DEV_GOOGLE_CLIENT_ID`, `DEV_GITHUB_CLIENT_SECRET`, etc.
   - Rotate repository secrets periodically (at least every 6 months)

3. **Use different credentials for dev and production**
   - Shared development OAuth apps for the team (stored in repository secrets)
   - Separate production OAuth apps (managed by deployment team)
   - This allows you to revoke/rotate credentials independently
   - Development apps should only work with `localhost:3000`

4. **Limit OAuth app permissions**
   - Only request the minimum necessary scopes
   - Current implementation uses default scopes (basic profile info)
   - Review OAuth app permissions regularly

5. **Use HTTPS in production**
   - Configure SSL/TLS certificates for your production domain
   - Update OAuth redirect URIs to use `https://`
   - Never use `http://` for production OAuth callbacks

6. **Secure the EC2 instance**
   - Restrict `.env` file permissions: `chmod 600 commitment/.env`
   - Ensure only authorized users have SSH access
   - Never expose `.env` contents in logs or error messages

7. **Personal OAuth Apps (if created)**
   - Delete your personal development OAuth apps when no longer needed
   - Don't share your personal OAuth credentials with other developers
   - Use repository secrets instead of creating personal OAuth apps

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
