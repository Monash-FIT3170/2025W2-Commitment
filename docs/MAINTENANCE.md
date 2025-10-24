# Maintenance Plan

## Table of Contents
- [Maintenance Plan](#maintenance-plan)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [System Architecture](#system-architecture)
    - [Frontend (Meteor Application)](#frontend-meteor-application)
    - [Backend (Haskell API)](#backend-haskell-api)
    - [Backend (Python Executor)](#backend-python-executor)
    - [Database (MongoDB Atlas)](#database-mongodb-atlas)
    - [Infrastructure (Docker + CI/CD)](#infrastructure-docker--cicd)
  - [Hosting and Deployment](#hosting-and-deployment)
    - [Nectar ARDC](#nectar-ardc)
    - [MongoDB Atlas](#mongodb-atlas)
    - [Domains](#domains)
  - [Post-Semester Plan](#post-semester-plan)
    - [Costs and Responsibilities](#costs-and-responsibilities)
  - [Maintenance and Update Recommendations](#maintenance-and-update-recommendations)
    - [Known Issues](#known-issues)
    - [Future Work](#future-work)
  - [Ending Notes](#ending-notes)


## Overview

This document outlines the maintenance and sustainability plan for the **Commitment** project.

The system integrates several core technologies:
- **Frontend:** Meteor + TypeScript
- **Backend:** Haskell API, Python Executor
- **Database:** MongoDB Atlas
- **Infrastructure:** Docker for containerization and GitHub Actions for CI/CD automation

This maintenance plan provides a clear record of the project’s operational setup, post-submission expectations, and guidance for any future contributors who may wish to continue development.

## System Architecture

### Frontend (Meteor Application)
- **Description:** A reactive client-server web application built using Meteor with TypeScript.
- **Responsibilities:**
  - Displays user interface components and handles client interactions.
  - Manages user sessions, routing, and API communication with the backend.
- **Deployment:** Packaged and deployed as a Docker container.
- **Key Maintenance Considerations:**
  - Future framework updates (Meteor, Node.js, and TypeScript).
  - Compatibility of Meteor packages and NPM dependencies.

### Backend (Haskell API)
- **Description:** REST API written in Haskell that fetches and processes data for the frontend.
- **Responsibilities:**
  - Performs data computation and provides structured responses to the Meteor app.
  - Acts as the intermediary between the frontend and external data sources.
- **Deployment:** Docker container that runs concurrently with the Meteor app in production.

### Backend (Python Executor)
- **Description:** REST API that can execute arbitrary python code.
- **Responsibilities:**
  - Executes user-provided python code for custom scaling strategies in a sandboxed environment.
- **Deployment:** Docker container that runs concurrently with the Meteor app in production.
- **Key Maintenance Considerations:**
  - Security requirements of executing arbitrary user-provided code.

### Database (MongoDB Atlas)
- **Description:** Cloud-hosted MongoDB instance storing persistent data, such as repository, user information, and alias configuration.
- **Responsibilities:**
  - Manages different collections.
  - Provides persistent storage independent of container lifecycles.
- **Maintenance Notes:**
  - The project currently uses the **free-tier cluster**.
  - Backup and scaling would need review if the project expands.

### Infrastructure (Docker + CI/CD)
- **Containerization:** Both frontend and backend are built into Docker images to ensure reproducible environments.
- **Orchestration:** `docker-compose` used for local and production setups.
- **CI/CD Pipeline:** GitHub Actions handles:
  - Type checking and linting.
  - Unit and integration tests.
  - Automatic image builds and deployment to production.
- **Deployment Scripts:** Included in the `README` with detailed setup steps.

## Hosting and Deployment
### Nectar ARDC

 Nectar ARDC (Australian Research Data Commons) provides free large scale computing infrastructure, software and data access to eligible Australian researchers, Monash students also fall under this. The Meteor Container and Haskell API Container are hosted on this solution.
- **Type:** Free student cloud allocation
- **Access:** Deployed using Docker Compose over SSH with environment variables stored securely.

### MongoDB Atlas
A fully managed cloud database service that simplifies deploying, managing, and scaling MongoDB databases. The data fetched from GitHub repositories is stored here. Steps for setting this up are found in the [Atlas Setup README](/docs/ATLAS_SETUP.md).
- **Type:** Free student cloud allocation
- **Access:** Hosted on Atlas, can be accessed via login or remotely through a Mongo connection.

### Domains
Currently the project uses [this domain](commitmentfit3170.net) to allow users to access the web application. This requires a domain to be purchased and a DNS registrar set up to resolve the domain name. Steps for this are outlined in the [Deployment README](/docs/DEPLOYMENT.md).

- **Domain Name:**
  - Domain name was purchased on [Squarespace Domains](https://domains.squarespace.com/?channel=pbr&subchannel=go&campaign=pbr-go-au-en-core_category-e&subcampaign=(domains-en_squarespace-domains_e)&gclsrc=aw.ds&&cid=16750987370&aid=133807081854&tid=aud-307746717000:kwd-95472161576&mt=e&eid=&loc_p_ms=9071445&loc_i_ms=&nw=g&d=c&adid=602205694451&channel2=pbr&subchannel2=go&gad_source=1&gad_campaignid=16750987370&gbraid=0AAAAADxS_FKppSxP1k2Gh0o6Z7rzi4HUd&gclid=CjwKCAjwu9fHBhAWEiwAzGRC_51ix7197voPgVy5CcTJ85GtTbS-E4L6lGuYsKi_XMXbj43svKEPShoCnmwQAvD_BwE)
  - Incurs a yearly cost that is registered to the Commitment email
- **DNS Registrar:**
  - Managed through [Cloudflare](https://www.cloudflare.com/en-au/application-services/products/dns/)
  - Uses the free-tier
  - Only accessibly via the Commitment email

## Post-Semester Plan

After submission:
- There will be **no designated maintainer or ongoing support**, the project will be abandoned after submission.
- The **GitHub repository** will still remain accessible for viewing and forking.
- Ongoing costs **will not be** maintained and all hosting solutions **will cease to run**.

All accounts, credentials and affiliated management details will be decomissioned, they will not be archived for security reasons. Any new contributor/maintainer must create new accounts and credentials in order to restart all services. Instructions for all setup are provided to users.

### Costs and Responsibilities

The below are all the related costs/responsibilities that are associated with the project. 
**Note that all of these will be decomissioned/abandoned at project completion.**

| Component          | Description                        | Cost                      | Responsibility                             |
| ------------------ | ---------------------------------- | ------------------------- | ------------------------------------------ |
| Nectar ARDC        | Cloud hosting for app containers   | Free (student allocation) | Expires post-semester                      |
| MongoDB Atlas      | Cloud database                     | Free Tier                 | Expires post-semester                      |
| Cloudflare DNS     | Managing DNS                       | Free Tier                 | Expires post-semester                      |
| Commitment Email   | Used as admin account for services | Free Google Account       | Abandoned/Deleted post-semester            |
| Squarespace Domain | Custom web domain                  | Annual Renewal Fee        | Paid via team email, expires on 08/10/2026 |
| GitHub Actions     | CI/CD automation                   | Free for Public Repos     | None                                       |


## Maintenance and Update Recommendations

Interested individuals or teams can:
  - Fork the project, setup, and redeploy independently.
  - Build upon existing functionality using Docker.
  - Extend the system using the defined API and architecture.

Relevant documentation for developers to read are below:

| README Name  | Description                                      | Link                               |
| ------------ | ------------------------------------------------ | ---------------------------------- |
| README       | Setup instructions and introduction to project.  | [View README](/README.md)          |
| DEPLOYMENT   | Instructions on setting up automated deployment. | [View README](/docs/DEPLOYMENT.md) |
| CONTRIBUTING | Details on code contribution standards.          | [View README](/CONTRIBUTING.md)    |


### Known Issues

Although the project has reached the concluding stage in its lifecycle, issues still remain with the current implementation of the system. Several issues have been identified by developers and testers that could not be addressed during the lifetime of the project. As such these issues still remain and can be addressed by any future developers. Issues are divided into either a `bug` or `feature`, these issues can be addressed by any future developers/maintainers.

All current issues can be found on the [repository issue page](https://github.com/Monash-FIT3170/2025W2-Commitment/issues).

### Future Work

Although no active maintenance is planned post-submission, the following updates should be performed if the project is continued:

**Framework Modernization**
- Frontend: Consider migrating from Meteor to a more modern framework such as Next.js or React + Vite for improved performance, modularity, and ecosystem support.
- Backend: Migrate from Haskell to a more maintainable REST or GraphQL backend such as Node.js (Express, NestJS) or Python (FastAPI) if future developers are less familiar with Haskell.

**Infrastructure & Deployment**
- Introduce environment-based deployments (staging vs production).
- Move hosting from Nectar to a long-term provider such as AWS.
- Improve the structure of the MongoDB collections.

**Testing and Quality Assurance**
- Increase test coverage in both Meteor and Haskell components.
- Integrate automated end-to-end (E2E) tests with tools such as Cypress or Playwright.
- Address all the linting and typecheck issues that remain in the repository.

**Long-Term Sustainability**
- Consider container orchestration (e.g., Docker Swarm or Kubernetes) if scaling becomes necessary.
- Implement automated database backups.
- Introduce structured logging (e.g., via Winston or Bunyan) and monitoring dashboards.

## Ending Notes

This project represents the collaborative effort of 15 dedicated contributors over a full academic year.  
For those who choose to continue its development, we encourage maintaining the same level of care, attention to detail, and collaborative spirit that defined its creation.  
While active maintenance will cease, the repository will remain as a record of best practices, teamwork, and innovation.