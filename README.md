<div align="center">
  <picture>
    <source media="(prefers-color-scheme: light)" srcset="docs/assets/logo.light.svg">
    <source media="(prefers-color-scheme: dark)" srcset="docs/assets/logo.dark.svg">
    <img src="docs/assets/logo.light.svg" width="625px" alt="Commitment logo">
  </picture>
</div>


# Table of Contents

- [Table of Contents](#table-of-contents)
- [2025W2-Commitment](#2025w2-commitment)
  - [System Requirements](#system-requirements)
  - [Environment Setup](#environment-setup)
    - [Docker Installation](#docker-installation)
      - [Select Target Platform](#select-target-platform)
      - [Install Docker Desktop](#install-docker-desktop)
      - [Install Verification](#install-verification)
    - [Atlas Mongo Setup](#atlas-mongo-setup)
    - [OAuth Setup](#oauth-setup)
    - [Building the Environment](#building-the-environment)
  - [Running Project](#running-project)
    - [Docker Commands](#docker-commands)
  - [Deployment](#deployment)
  - [Maintenance](#maintenance)
  - [Other Useful Reading](#other-useful-reading)
  - [Contributors ✨](#contributors-)
    - [System Architects](#system-architects)
    - [Product Managers](#product-managers)
    - [Release Train Engineers](#release-train-engineers)
  - [Generative AI Statement](#generative-ai-statement)

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

### Atlas Mongo Setup

This project uses the Atlas service to host the database. Instructions can be found in [this README](/docs/ATLAS_SETUP.md).
Instructions on what to put in the `.env` file are also provided.

### OAuth Setup

Third-party authentication is setup for user sign in. Instructions for setting this up can be found in [this README](/docs/OAUTH_SETUP.md).
Instructions on what to put in the `.env` file are also provided.

### Building the Environment

These next steps are required to build and spin up the Docker image that will be used.

1. Create the `.env` file for Mongo connection in the `commitment` directory, example show below.

```bash
MONGO_URL=mongodb+srv://<user>:<password>@database.c8q1uxt.mongodb.net/<database>?retryWrites=true&w=majority&appName=<Database>
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

## Deployment

This project is being hosted on a custom deployment solution. Steps for deployment setup can be found in [this README](/docs/DEPLOYMENT.md).

## Maintenance

This project will not be actively maintained at the conclusion of the second 2025 semester. More information and steps for future maintainers (and contributors) can be found in [this README](/docs/MAINTENANCE.md).

## Other Useful Reading

Below are useful links for learning a little more about the project from the READMEs scattered throughout.
| README Name        | Info / Description                                             | Link                                                 |
| ------------------ | -------------------------------------------------------------- | ---------------------------------------------------- |
| Coding Standards   | Quick guide to coding standards in the repository.             | [View README](docs/CODING_STANDARDS.md)              |
| Mongo Setup        | More information on playing with Mongo.                        | [View README](commitment/atlas/README_DATA_ENTRY.md) |
| Mongo Architecture | Documentation on the architecture of Mongo in our application. | [View README](docs/ARCHITECTURE.md)                  |
| Tailwind How-To    | Example usage and demos for applying tailwind in the project.  | [View README](docs/TAILWIND_COLOURS.md)              |
| ESLint Guide       | Usage and documentation of eslint in the project.              | [View README](docs/ESLINT.md)                        |
| Contribution Guide | Guidelines for contributing to the project.                    | [View README](CONTRIBUTING.md)                       |

## Contributors ✨

Thanks goes to these wonderful people that completed this project for **FIT3170 (Semester 2, 2025)** ([emoji key](https://allcontributors.org/docs/en/emoji-key)):
- All contributors participated in development, documentation, and UI design.
- Role distinctions are based on each member chosen role at the start of the FIT3170 unit, following [SAFe](https://www.atlassian.com/agile/agile-at-scale/what-is-safe).
- To view detailed contributions, visit the [GitHub repository’s contribution breakdown](https://github.com/Monash-FIT3170/2025W2-Commitment/graphs/contributors).
.

### System Architects

System Architects were responsible for the technical architecture, software design patterns, and infrastructure decisions. They ensured scalability, maintainability, and adherence to clean coding standards (including consistent variable naming, code commenting, and documentation practices).

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tr>
    <td align="center"><a href="https://github.com/AroshHeenkenda"><img src="https://github.com/AroshHeenkenda.png" width="100px;" alt=""/><br /><sub><b>Arosh Heenkenda</b></sub></a><br /><a href="#code-arosh" title="Code">💻</a> <a href="#doc-arosh" title="Documentation">📖</a> <a href="#design-arosh" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/BaileyChessum"><img src="https://github.com/BaileyChessum.png" width="100px;" alt=""/><br /><sub><b>Bailey Chessum</b></sub></a><br /><a href="#code-bailey" title="Code">💻</a> <a href="#doc-bailey" title="Documentation">📖</a> <a href="#design-bailey" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/QodeWiz"><img src="https://github.com/QodeWiz.png" width="100px;" alt=""/><br /><sub><b>Ishrat (Isha) Kaur</b></sub></a><br /><a href="#code-isha" title="Code">💻</a> <a href="#doc-isha" title="Documentation">📖</a> <a href="#design-isha" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/Densetsu152637"><img src="https://github.com/Densetsu152637.png" width="100px;" alt=""/><br /><sub><b>Nicholas Bisset</b></sub></a><br /><a href="#code-nicholas" title="Code">💻</a> <a href="#doc-nicholas" title="Documentation">📖</a> <a href="#design-nicholas" title="Design">🎨</a></td>
  </tr>
</table>
<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->

### Product Managers

Product Managers oversaw project direction, user requirements, and alignment with stakeholder needs. They managed backlog prioritisation, coordinated user testing, and ensured the product met design and functionality goals.

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tr>
    <td align="center"><a href="https://github.com/AmyTjea"><img src="https://github.com/AmyTjea.png" width="100px;" alt=""/><br /><sub><b>Amy Tjea</b></sub></a><br /><a href="#code-amy" title="Code">💻</a> <a href="#doc-amy" title="Documentation">📖</a> <a href="#design-amy" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/yeszab"><img src="https://github.com/yeszab.png" width="100px;" alt=""/><br /><sub><b>Azab Azab</b></sub></a><br /><a href="#code-azab" title="Code">💻</a> <a href="#doc-azab" title="Documentation">📖</a> <a href="#design-azab" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/milnia4"><img src="https://github.com/milnia4.png" width="100px;" alt=""/><br /><sub><b>Milni Abeysekara</b></sub></a><br /><a href="#code-milni" title="Code">💻</a> <a href="#doc-milni" title="Documentation">📖</a> <a href="#design-milni" title="Design">🎨</a></td>
  </tr>
  <tr>
    <td align="center"><a href="https://github.com/Rishardjaf"><img src="https://github.com/Rishardjaf.png" width="100px;" alt=""/><br /><sub><b>Rishard Jafferjee</b></sub></a><br /><a href="#code-rishard" title="Code">💻</a> <a href="#doc-rishard" title="Documentation">📖</a> <a href="#design-rishard" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/occabatd"><img src="https://github.com/occabatd.png" width="100px;" alt=""/><br /><sub><b>Undram (Occa) Batdelger</b></sub></a><br /><a href="#code-occa" title="Code">💻</a> <a href="#doc-occa" title="Documentation">📖</a> <a href="#design-occa" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/munasve"><img src="https://github.com/munasve.png" width="100px;" alt=""/><br /><sub><b>Venuka Munasinghe</b></sub></a><br /><a href="#code-venuka" title="Code">💻</a> <a href="#doc-venuka" title="Documentation">📖</a> <a href="#design-venuka" title="Design">🎨</a></td>
  </tr>
</table>
<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->

### Release Train Engineers

Release Train Engineers acted as a servant leader and coach for an Agile Release Train (ART) within the Scaled Agile Framework, ensuring multiple teams work together to deliver value. Their main responsibilities include facilitating key ART events like PI Planning, removing impediments, managing risks, and driving continuous improvement across the train.

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tr>
    <td align="center"><a href="https://github.com/benmcshanag13"><img src="https://github.com/benmcshanag13.png" width="100px;" alt=""/><br /><sub><b>Ben McShanag</b></sub></a><br /><a href="#code-ben" title="Code">💻</a> <a href="#doc-ben" title="Documentation">📖</a> <a href="#design-ben" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/Feenerys"><img src="https://github.com/Feenerys.png" width="100px;" alt=""/><br /><sub><b>Chhordapitou (Finn) Nguon</b></sub></a><br /><a href="#code-finn" title="Code">💻</a> <a href="#doc-finn" title="Documentation">📖</a> <a href="#design-finn" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/janidu04"><img src="https://github.com/janidu04.png" width="100px;" alt=""/><br /><sub><b>Janidu Hathurusinghe</b></sub></a><br /><a href="#code-janidu" title="Code">💻</a> <a href="#doc-janidu" title="Documentation">📖</a> <a href="#design-janidu" title="Design">🎨</a></td>
  </tr>
  <tr>
    <td align="center"><a href="https://github.com/YoonusNazz"><img src="https://github.com/YoonusNazz.png" width="100px;" alt=""/><br /><sub><b>Muhammad (Yoonus) Nazeem</b></sub></a><br /><a href="#code-yoonus" title="Code">💻</a> <a href="#doc-yoonus" title="Documentation">📖</a> <a href="#design-yoonus" title="Design">🎨</a></td>
    <td align="center"><a href="https://github.com/sabo0007"><img src="https://github.com/sabo0007.png" width="100px;" alt=""/><br /><sub><b>Sami Abou Zolf</b></sub></a><br /><a href="#code-sami" title="Code">💻</a> <a href="#doc-sami" title="Documentation">📖</a> <a href="#design-sami" title="Design">🎨</a></td>
  </tr>
</table>
<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->

## Generative AI Statement

Generative AI tools were utilised throughout the development process to support tasks such as code drafting and documentation refinement. All AI-generated content was critically reviewed and verified by the project team to ensure accuracy, originality, and alignment with project requirements.
