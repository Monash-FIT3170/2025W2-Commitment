# Table of Contents

- [Table of Contents](#table-of-contents)
- [2025W2-Commitment](#2025w2-commitment)
  - [System Requirements](#system-requirements)
  - [Environment Setup](#environment-setup)
    - [Docker Installation](#docker-installation)
      - [Select Target Platform](#select-target-platform)
      - [Install Docker Desktop](#install-docker-desktop)
      - [Install Verification](#install-verification)
    - [Atlas Database Setup](#atlas-database-setup)
      - [Creating an Atlas Account](#creating-an-atlas-account)
      - [Setting Up Your Cloud Database](#setting-up-your-cloud-database)
    - [Building the Environment](#building-the-environment)
  - [Running Project](#running-project)
    - [Docker Commands](#docker-commands)
- [Deployment](#deployment)
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
  - [Other Useful Reading](#other-useful-reading)
  - [Contributors ✨](#contributors-)

# 2025W2-Commitment

**Commitment** is a web-based GUI tool that provides **code contribution analysis** and **automatic scaling suggestions** for student assessments.

With Commitment, you can:

- Enter a Git URL to view repository stats or bookmark repos (when signed in).
- Explore contributor metrics (Lines of Code, Lines of Code per Commit, Commits per Day, Total Number of commits) filtered by branch and timeframe.
- Generate recommended assessment scalings using a variety of strategies (percentiles, mean + standard deviation, quartiles, etc.).
- Visualize metrics and recommendations through interactive graphs.
- Upload a Moodle grading sheet to automatically apply scaling before downloading it for submission.
- Filter out unwanted commits (e.g., merges or keyword-based).
- Map multiple Git accounts to the same contributor via a repo-specific config file.

Commitment is designed to support fairer and more transparent grading in team-based coding projects.

## System Requirements

To run this project with **Docker**, **Meteor**, and **Haskell**, to ensure a smooth experience running your development environment must meet the following requirements:

- **OS**: Linux (Ubuntu 22.04+) or macOS (Intel/Apple Silicon), or Windows 10/11 Pro with WSL2
- **CPU**: **2 Cores** minimum, **Quad-core 64-bit processor** (Intel i5/i7, AMD Ryzen 5/7, or Apple M1/M2) strongly recommended
- **RAM**: **8 GB** minimum, **16 GB+** strongly recommended
- **Disk**: **16 GB Free** minimum, **32 GB+** strongly recommended (fast storage for builds and containers)

## Environment Setup

Follow the below steps to set up your Docker environment for development.

### Docker Installation

Docker Desktop can be installed for all platforms from the official website, [Docker Desktop for Windows](https://www.docker.com/products/docker-desktop/) (Scroll Below _Choose Plan_).

#### Select Target Platform

Depending on your device you can choose the target platform:

- Download for Mac - **Apple Silicon**
- Download for Mac - **Intel Chip**
- Download for Windows AMD64 - **Windows (64-bit)**
- Download for Windows ARM64 - **Snapdragon Laptops**
- Download for Linux - **Ubuntu, Debian, Fedora, etc.**

#### Install Docker Desktop

1. Open the downloaded installer: **Docker Desktop Installer exe**.
2. Follow the installation prompts.
3. Enable WSL2 Backend (recommended).
4. Restart device to complete installation.

#### Install Verification

1. Open native terminal on device.
2. Run `docker --version` to verify the installation.

_If there are any issues please contact an SA for help._

### Atlas Database Setup

To run this project, you will need to set up a MongoDB Atlas cloud database. Follow these steps to create your Atlas account and configure your database.

#### Creating an Atlas Account

1. Navigate to [MongoDB Atlas](https://www.mongodb.com/atlas) in your web browser.
2. Click **"Try Free"** or **"Start Free"** to begin the registration process.
3. Fill out the registration form with your email address and create a secure password.
4. Verify your email address through the confirmation email sent by MongoDB.
5. Complete your profile setup and accept the terms of service.

#### Setting Up Your Cloud Database

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

### Building the Environment

These next steps are required to build and spin up the Docker image that will be used.

1. Create the `.env` file for Mongo connection in the `commitment` directory, example show below.

```
MONGO_URL=mongodb+srv://<user>:<password>@database.c8q1uxt.mongodb.net/<database>?retryWrites=true&w=majority&appName=<Database>
SERVER_HOST=0.0.0.0
PORT=3000
DB_NAME=commitment_db
NODE_ENV=development
```

2. Ensure that you are in the **root directory** of the project and open a terminal.
3. Run `docker-compose build` to build the image.
   - This may take some time!
4. Run `docker-compose up -d` to spin it up and attach to a container.
5. The container is ready for development.

## Running Project

These provide steps to run the project.

1. In the root directory of the project start the container with `docker-compose start`.
2. Once it has started enter the `3170-build` container using `docker exec -it 3170-build bash`, this should take you into the `\commitment` directory.
3. Run `npm install` to install required dependencies, this may take some time.
4. Run `meteor` to run the application.
   - This will be mapped to your `localhost` on `PORT 3000`.
5. Enjoy developing!

### Docker Commands

These commands are useful for working with the container but must be run from the root directory of the project.

| Use                          | Command                            |
| ---------------------------- | ---------------------------------- |
| Start Container              | `docker-compose start`             |
| Stop Container               | `docker-compose stop`              |
| Access Dev Container         | `docker exec -it 3170-build bash`  |
| Access Haskell Container     | `docker exec -it haskell-api bash` |
| Attach Container to Terminal | `docker attach <container>`        |

# Deployment

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
  ```
Now the everything is setup for deploment. Pushes to main will trigger the workflow for deployment.

## Other Useful Reading

Below are useful links for learning a little more about the project from the READMEs scattered throughout.
| README Name        | Info / Description                                             | Link                                                 |
| ------------------ | -------------------------------------------------------------- | ---------------------------------------------------- |
| Mongo Setup        | More information on playing with Mongo.                        | [View README](commitment/atlas/README_DATA_ENTRY.md) |
| Mongo Architecture | Documentation on the architecture of Mongo in our application. | [View README](commitment/ARCHITECTURE.md)            |
| Tailwind How-To    | Example usage and demos for applying tailwind in the project.  | [View README](docs/TAILWIND_COLOURS.md)              |
| ESLint Guide       | Usage and documentation of eslint in the project.              | [View README](docs/ESLINT.md)                        |
| Contribution Guide | Guidelines for contributing to the project                     | [View README](CONTRIBUTING.md)                       |

## Contributors ✨

Thanks goes to these wonderful people ([emoji key](https://allcontributors.org/docs/en/emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tr>
    <td align="center"><a href="https://github.com/AmyTjea"><img src="https://github.com/AmyTjea.png" width="100px;" alt=""/><br /><sub><b>Amy Tjea</b></sub></a><br /><a href="#code-amy" title="Code">💻</a> <a href="#doc-amy" title="Documentation">📖</a> <a href="#design-amy" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/AroshHeenkenda"><img src="https://github.com/AroshHeenkenda.png" width="100px;" alt=""/><br /><sub><b>Arosh Heenkenda</b></sub></a><br /><a href="#code-arosh" title="Code">💻</a> <a href="#doc-arosh" title="Documentation">📖</a> <a href="#design-arosh" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/yeszab"><img src="https://github.com/yeszab.png" width="100px;" alt=""/><br /><sub><b>Azab Azab</b></sub></a><br /><a href="#code-azab" title="Code">💻</a> <a href="#doc-azab" title="Documentation">📖</a> <a href="#design-azab" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/BaileyChessum"><img src="https://github.com/BaileyChessum.png" width="100px;" alt=""/><br /><sub><b>Bailey Chessum</b></sub></a><br /><a href="#code-bailey" title="Code">💻</a> <a href="#doc-bailey" title="Documentation">📖</a> <a href="#design-bailey" title="Design">🎨</a></td>
  </tr>
  <tr>
    <td align="center"><a href="https://github.com/benmcshanag13"><img src="https://github.com/benmcshanag13.png" width="100px;" alt=""/><br /><sub><b>Ben McShanag</b></sub></a><br /><a href="#code-ben" title="Code">💻</a> <a href="#doc-ben" title="Documentation">📖</a> <a href="#design-ben" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/Feenerys"><img src="https://github.com/Feenerys.png" width="100px;" alt=""/><br /><sub><b>Chhordapitou (Finn) Nguon</b></sub></a><br /><a href="#code-finn" title="Code">💻</a> <a href="#doc-finn" title="Documentation">📖</a> <a href="#design-finn" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/Densetsu152637"><img src="https://github.com/Densetsu152637.png" width="100px;" alt=""/><br /><sub><b>Nicholas Bisset</b></sub></a><br /><a href="#code-nicholas" title="Code">💻</a> <a href="#doc-nicholas" title="Documentation">📖</a> <a href="#design-nicholas" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/occabatd"><img src="https://github.com/occabatd.png" width="100px;" alt=""/><br /><sub><b>Undram (Occa) Batdelger</b></sub></a><br /><a href="#code-occa" title="Code">💻</a> <a href="#doc-occa" title="Documentation">📖</a> <a href="#design-occa" title="Design">🎨</a></td>
  </tr>
  <tr>
    <td align="center"><a href="https://github.com/QodeWiz"><img src="https://github.com/QodeWiz.png" width="100px;" alt=""/><br /><sub><b>Ishrat (Isha) Kaur</b></sub></a><br /><a href="#code-isha" title="Code">💻</a> <a href="#doc-isha" title="Documentation">📖</a> <a href="#design-isha" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/milnia4"><img src="https://github.com/milnia4.png" width="100px;" alt=""/><br /><sub><b>Milni Abeysekara</b></sub></a><br /><a href="#code-milni" title="Code">💻</a> <a href="#doc-milni" title="Documentation">📖</a> <a href="#design-milni" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/Rishardjaf"><img src="https://github.com/Rishardjaf.png" width="100px;" alt=""/><br /><sub><b>Rishard Jafferjee</b></sub></a><br /><a href="#code-rishard" title="Code">💻</a> <a href="#doc-rishard" title="Documentation">📖</a> <a href="#design-rishard" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/sabo0007"><img src="https://github.com/sabo0007.png" width="100px;" alt=""/><br /><sub><b>Sami Abou Zolf</b></sub></a><br /><a href="#code-sami" title="Code">💻</a> <a href="#doc-sami" title="Documentation">📖</a> <a href="#design-sami" title="Design">🎨</a></td>
  </tr>
  <tr>
    <td align="center"><a href="https://github.com/munasve"><img src="https://github.com/munasve.png" width="100px;" alt=""/><br /><sub><b>Venuka Munasinghe</b></sub></a><br /><a href="#code-venuka" title="Code">💻</a> <a href="#doc-venuka" title="Documentation">📖</a> <a href="#design-venuka" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/YoonusNazz"><img src="https://github.com/YoonusNazz.png" width="100px;" alt=""/><br /><sub><b>Muhammad (Yoonus) Nazeem</b></sub></a><br /><a href="#code-yoonus" title="Code">💻</a> <a href="#doc-yoonus" title="Documentation">📖</a> <a href="#design-yoonus" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/janidu04"><img src="https://github.com/janidu04.png" width="100px;" alt=""/><br /><sub><b>Janidu Hathurusinghe</b></sub></a><br /><a href="#code-janidu" title="Code">💻</a> <a href="#doc-janidu" title="Documentation">📖</a> <a href="#design-janidu" title="Design">🎨</a></td>
  </tr>
</table>
<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

<!-- ALL-CONTRIBUTORS-LIST:END -->
