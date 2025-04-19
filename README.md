# 2025W2-Commitment
## Name List
- Arosh Heenkenda
- Bailey Chessum
- Nicholas Bisset
- Ben McShanag
- Undram (Occa) Batdelger
- Milni Abeysekara 
- Amy Tjea
- Janidu Hathurusinghe
- Muhammad Yoonus Nazeem
- Ishrat (Isha) Kaur 
- Venuka Munasinghe
- Rishard Jafferjee
- Sami Abou Zolf
- Azab Azab
- Chhordapitou (Finn) Nguon

## Environment Setup
Follow the below steps to set up your Docker environment for development.

### Docker Installation
Docker Desktop can be installed for all platforms from the official website, [Docker Desktop for Windows](https://www.docker.com/products/docker-desktop/) (Scroll Below *Choose Plan*).

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

*If there are any issues please contact an SA for help.*

### Building the Environment
These next steps are required to build and spin up the Docker image that will be used. 
1. Ensure that you are in the **root directory** of the project and open a terminal.
2. Run `docker-compose build` to build the image.
    - This may take some time!
3. Run `docker-compose up -d` to spin it up and attach to a container.
4. The container is ready for development.

### Docker Commands
These commands are useful for working with the container but must be run from the root directory of the project.

| Use | Command |
|---|---|
| Start Container | `docker-compose start` |
| Stop Container  | `docker-compose stop` |
| Access Dev Container | `docker exec -it 3170-env bash` |
| Access Mongo Container | `docker exec -it mongo bash` |

## Running Project
These provide steps to run the project.
1. In the root directory of the project start the container.
2. Once it has started enter the `3170-env` container, this should take you into the `\commitment` directory.
3. [OPTIONAL] Run `npm install` to install dependencies. 
    - If any new project dependencies have been added since the container was made this will need to be run.
    - If not, you can skip this step.
4. Run `meteor` to run the application.
    - This will be mapped to your `localhost` on `PORT 3000`.
