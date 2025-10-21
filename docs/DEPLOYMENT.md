# Deployment
## Table of Contents
- [Deployment](#deployment)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Initial Setup](#initial-setup)
    - [1. Acquire Instance](#1-acquire-instance)
    - [2. Dockerhub Setup](#2-dockerhub-setup)
    - [3. Github Setup](#3-github-setup)
    - [4. Domain \& DNS Setup (Optional)](#4-domain--dns-setup-optional)
  - [Instance Configuration](#instance-configuration)
    - [1. Docker Installation](#1-docker-installation)
    - [2. NGINX Setup](#2-nginx-setup)
    - [(Optional) SSL Certbot Auth](#optional-ssl-certbot-auth)
    - [3. Repository Setup](#3-repository-setup)


## Overview

At the conclusion of **Semester 2 (2025) at Monash University**, the Commitment web app is accessible [via this url](https://commitmentfit3170.net/).

For future contributions to this repository there are a set of steps needed to follow to set up deployment via the Github actions. Changes will be pushed to the production site whenever a PR is merged into `main` through the Github Actions pipeline.

## Initial Setup
### 1. Acquire Instance

An instance in this case is some virtual machine that will be accessed and used to host the application. There are a number of services that offer machine instances for use in this purpose.

Some examples include:
- [Amazon Web Services](https://aws.amazon.com/ec2/instance-types/)
- [Digital Ocean](https://www.digitalocean.com/products/droplets)
- [Azure](https://azure.microsoft.com/en-us/products/virtual-machines)

There a number of options available but must meet a set of system requirements.

**Minimum System Requirements**
- **Flavour**: Ubuntu 22.04 LTS (Jammy)
- **Cores**: 2
- **RAM**: 4 GB
- **Storage**: 15 GB

Once an instance has been acquired it will primarily be accessed via [SSH](https://www.ssh.com/academy/ssh/protocol). To do this a service will provide you with 3 things:
- **Host Name**: An IP Address or DNS.
- **User**: The user of the account you want to log in to (should just be ubuntu in this case).
- **Identity or Key File**: A file with an encrypted key to verify your identity when remoting in.

You can SSH onto a server with the following command:
``` bash
ssh [user]@[hostname] -i [path to identity file]
```

Have those 3 things handy as it will be required in later steps.


### 2. Dockerhub Setup
Dockerhub is where the Docker images will be pushed and used in the production bundle on the instance. 

1. Navigate to [Dockerhub](https://hub.docker.com/) and create an account.
2. Take note of the username that you create.
3. Create a Personal Access Token following the [instructions here](https://docs.docker.com/security/access-tokens/).
4. Take note of the PAT for later use.

### 3. Github Setup
Now you should have 5 things which need to be added as secrets to this repository as they will be used in the `deployment.yml` file. You can follow [these instructions](https://docs.github.com/en/codespaces/managing-codespaces-for-your-organization/managing-development-environment-secrets-for-your-repository-or-organization) to add the following secrets to the repository.

| Secret Name          | Info               |
| -------------------- | ------------------ |
| `NECTAR_HOST`        | Instance Host Name |
| `NECTAR_USER`        | Instance User Name |
| `NECTAR_KEY`         | Instance SSH Key   |
| `DOCKERHUB_USERNAME` | Dockerhub Username |
| `DOCKERHUB_TOKEN`    | Dockerhub PAT      |

The names should match exactly as they are in the `deployment.yml` file. If you decide to rename them the file should also reflect those changes.

### 4. Domain & DNS Setup (Optional)
This step is optional, however, if you want your deployed app to have a domain it needs to be acquired. Additionally a service needs to be used to act as a DNS registrar to resolve the instance IP to the domain name.

This project used [Cloudflare](https://developers.cloudflare.com/fundamentals/manage-domains/) to manage the DNS and the domain was purchased on [Squarespace](https://domains.squarespace.com/). 

If this step is completed keep note of the domains that were acquired for later.

## Instance Configuration
The following sections cover the setup and required installations for the instance to run the deployed application. Ensure you have established an SSH connection to the instance before completing the next set of steps.

### 1. Docker Installation
Docker and its associated tools need to be installed on the instance to allow for the containerised deployment. Instructions for installation on Ubuntu machines can be found [here](https://docs.docker.com/engine/install/ubuntu/), however, the steps are provided below.

1. Uninstall older Docker Engine versions.
  ``` bash
  for pkg in docker.io docker-doc docker-compose docker-compose-v2 podman-docker containerd runc; do sudo apt-get remove $pkg; done
  ```

2. Set up the Docker `apt` repository.
  ``` bash
  # Add Docker's official GPG key:
  sudo apt-get update
  sudo apt-get install ca-certificates curl
  sudo install -m 0755 -d /etc/apt/keyrings
  sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
  sudo chmod a+r /etc/apt/keyrings/docker.asc

  # Add the repository to Apt sources:
  echo \
    "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
    $(. /etc/os-release && echo "${UBUNTU_CODENAME:-$VERSION_CODENAME") stable" | \
    sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
  sudo apt-get update
```
3. Install latest Docker packages.
  ``` bash
  sudo apt-get install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
  ```
4. Verify the installation.
  ``` bash
  # Verify running status
  sudo systemctl status docker
  # Some systems may need manual start
   sudo systemctl start docker
  ```
### 2. NGINX Setup
NGINX is used to act as a web server for serving the content of the web application and acting as a reverse proxy to forward requests. It is used to server our web app and route traffic through the domain name instead of pointing to the IP address.
1. Install nginx.
  ``` bash
  sudo apt update
  sudo apt upgrade -y
  sudo apt install nginx
  ```
2. Adjust the firewall, for HTTP and HTTPS.
  ``` bash
  sudo ufw allow 'Nginx Full'
  # Verify status
  sudo ufw status
  ```
3. From the root directory create a new nginx configuration file.
  ``` bash
  sudo nano /etc/nginx/sites-available/commitment.conf
  ```
4. Update the configuration file with the below contents. This will redirect and manage traffic to the server. Replace `app_domains` with all the domain names you may have acquired, also replace `host_ip` with the IP of the server.
  ``` bash
  server {
      listen 80;
      server_name app_domains host_ip;
      location /api/ {
          proxy_pass http://127.0.0.1:8081/;
          proxy_http_version 1.1;
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection 'upgrade';
          proxy_set_header Host $host;
          proxy_cache_bypass $http_upgrade;
      }
      location / {
          proxy_pass http://127.0.0.1:3000/;
          proxy_http_version 1.1;
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection 'upgrade';
          proxy_set_header Host $host;
          proxy_cache_bypass $http_upgrade;
      }
  ```
5. Enable the changes on nginx.
  ``` bash
  sudo ln -s /etc/nginx/sites-available/meteor /etc/nginx/sites-enabled/
  sudo nginx -t
  sudo systemctl restart nginx
  ```
### (Optional) SSL Certbot Auth
This optional step only applies if domain names have been acquired. This step will setup SSL for secure connections to the webapp using [Certbot](https://certbot.eff.org/). 
1. Install certbot on the instance.
  ``` bash
  sudo apt install certbot python3-certbot-nginx -y
  ```
2. Associate certification with domain names (will not work for IP address). Replace `your-domains` with any domains you have acquired.
  ``` bash
  sudo certbot --nginx -d you-domains
  ```
### 3. Repository Setup
These next steps will have the repository set up initially before any pipelines can be run.
1. Clone the repository into the root directory.
  ``` bash
  git clone https://github.com/Monash-FIT3170/2025W2-Commitment.git
  ```
2. Navigate to the commitment directory.
  ``` bash
  cd 2025W2-Commitment/commitment
  ```
3. Create a `.env` file, to be used during deployment.
  ``` bash
  sudo nano .env
  ```
4. Populate with the below information.
  ``` bash
  MONGO_URL=<YOUR_MONGO_URI>
  SERVER_HOST=0.0.0.0
  PORT=3000
  DB_NAME=commitment_db
  NODE_ENV=production
  ROOT_URL=<YOUR_DOMAIN_NAME>
  API_CONN_ENDPOINT=haskell-api:8081

  # OAuth Configuration (your personal dev credentials)
  GOOGLE_CLIENT_ID=your_google_client_id_here
  GOOGLE_CLIENT_SECRET=your_google_client_secret_here
  GITHUB_CLIENT_ID=your_github_client_id_here
  GITHUB_CLIENT_SECRET=your_github_client_secret_here
  ```
Now the everything is setup for deploment. Pushes to main will trigger the workflow for deployment.
