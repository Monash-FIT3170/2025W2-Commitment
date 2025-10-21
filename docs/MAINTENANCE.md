# Maintenance Plan

## Overview

This document outlines the maintenance and sustainability plan for the **Commitment** project.

The system integrates several core technologies:
- **Frontend:** Meteor + TypeScript
- **Backend:** Haskell API
- **Database:** MongoDB Atlas
- **Infrastructure:** Docker for containerization and GitHub Actions for CI/CD automation

This maintenance plan provides a clear record of the project’s operational setup, post-submission expectations, and guidance for any future contributors who may wish to continue development.

---

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

### Database (MongoDB Atlas)
- **Description:** Cloud-hosted MongoDB instance storing persistent data, such as repository and user information.
- **Responsibilities:**
  - Manages user and repository collections.
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

---

## Hosting and Deployment

### Current Hosting Environment
- **Provider:** Nectar ARDC (Australian Research Data Commons)
- **Type:** Free student cloud allocation
- **Services Running:**
  - Meteor container (frontend)
  - Haskell API container (backend)
  - MongoDB Atlas (remote database)
- **Access:** Deployed using Docker Compose over SSH with environment variables stored securely.

### Post-Semester Plan
- The **Nectar instance will be terminated** after the semester ends as part of the student allocation lifecycle.
- **Domain Name:** Purchased via Squarespace and linked to the app.  
  - Renewal required **annually** to maintain active access.
  - Managed through the Commitment team email account.

### Redeployment Instructions
Deployment can be reproduced following the steps in the project’s `README`. All environment variables (`.env`), database URIs, and API endpoints are defined in the documentation.

---

## Costs and Responsibilities

| Component          | Description                      | Cost                      | Responsibility        |
| ------------------ | -------------------------------- | ------------------------- | --------------------- |
| Nectar ARDC        | Cloud hosting for app containers | Free (student allocation) | Expires post-semester |
| MongoDB Atlas      | Cloud database (Free tier)       | Free                      | No ongoing charges    |
| Squarespace Domain | Custom web domain                | Annual renewal fee        | Paid via team email   |
| GitHub Actions     | CI/CD automation                 | Free for public repos     | None                  |

After Week 12, **no team member is responsible for continuing costs or maintenance**.  
The domain may expire if not renewed.

---

## Maintenance and Update Recommendations

Although no active maintenance is planned post-submission, the following updates should be performed if the project is continued:

### Framework and Dependencies
- **Meteor:** Regularly update to newer versions to maintain security and compatibility.
- **Node.js & TypeScript:** Keep aligned with current LTS releases.
- **Haskell API:** Rebuild against the latest library dependencies and compiler version (GHC).

### Security
- Rotate credentials stored in environment variables.
- Regularly review MongoDB Atlas access rules (IP whitelist, database users).
- Update base Docker images to minimize vulnerabilities.

### Infrastructure Improvements
- Consider moving to a long-term hosting provider (e.g., AWS, Render, or Railway).
- Automate deployment pipelines with environment-based branching.
- Enable monitoring/logging using Docker Healthchecks or Prometheus.

---

## Access and Credentials

All project credentials (MongoDB Atlas, Squarespace, and Nectar ARDC) are stored securely and are not publicly shared in the repository.

If credentials are required for redeployment or maintenance, they can be obtained by **contacting one of the System Architect contributors** from the original development team.

- **GitHub Repository:** Public access (read-only).
- **Forking:** Permitted for anyone interested in continuing development.
- **Secrets:** Stored in GitHub Actions for build and deploy pipelines.

---

## Handover and Continuity

After submission:
- There will be **no designated maintainer or ongoing support**.
- The **GitHub repository** will remain accessible for viewing and forking.
- All deployment and setup instructions are preserved in the README.
- Interested individuals or teams can:
  - Fork the project and redeploy independently.
  - Build upon existing functionality using Docker.
  - Extend the system using the defined API and architecture.

Future contributors are encouraged to:
- Address existing **open issues** on GitHub.
- Explore **migration paths** to modern frameworks (e.g., Next.js, Express, or FastAPI).
- Refactor code for modularity and long-term scalability.

---

## Future Recommendations

To ensure long-term viability, maintainability, and modernization, the following recommendations are suggested:

1. Framework Modernization
   Frontend: Consider migrating from Meteor to a more modern framework such as Next.js or React + Vite for improved performance, modularity, and ecosystem support.
   Backend: Migrate from Haskell to a more maintainable REST or GraphQL backend such as Node.js (Express, NestJS) or Python (FastAPI) if future developers are less familiar with Haskell.

2. Infrastructure & Deployment
    Replace manual deployment scripts with a CI/CD pipeline that automates:
       - Container image publishing to a registry.
       - Environment-based deployments (staging vs production).
       - Rollbacks and monitoring.
    
    Move hosting from Nectar to a long-term provider:
       - Render, Fly.io, or Railway for container-based hosting.

    MongoDB Atlas can remain as-is.

3. Testing and Quality Assurance
   Increase test coverage in both Meteor and Haskell components.
   Integrate automated end-to-end (E2E) tests with tools such as Cypress or Playwright.
   Add continuous testing to GitHub Actions pipelines.

4. Documentation and Developer Onboarding
    Expand the README with:
       - Architectural diagrams.
       - API endpoint documentation.
       - Add a developer onboarding guide with setup scripts for local testing.

5. Security and Performance
    Regularly scan Docker images for vulnerabilities.
    Enable rate limiting or authentication on the API layer.

6. Long-Term Sustainability
   Consider container orchestration (e.g., Docker Swarm or Kubernetes) if scaling becomes necessary.
   Implement automated database backups.
   Introduce structured logging (e.g., via Winston or Bunyan) and monitoring dashboards.


## Summary

- **Post-Semester Ownership:** None (project handed over as-is).  
- **Hosting:** Nectar ARDC instance (to be terminated after semester).  
- **Database:** MongoDB Atlas Free Tier (no ongoing costs).  
- **Domain:** Squarespace, requires yearly renewal.  
- **Deployment:** Fully automated with GitHub Actions and Docker Compose.  
- **Documentation:** Comprehensive setup and deployment guides in the README.  

This plan ensures that any future developer can easily reproduce, redeploy, or extend the system with minimal friction, even after the original team’s departure.
````

Would you like me to add a **“Future Recommendations” section** (e.g., migrating away from Meteor, adopting microservices, improving test coverage), or keep this as your final version?
