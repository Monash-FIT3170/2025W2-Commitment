# 2025W2-Commitment

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

### Building the Environment

These next steps are required to build and spin up the Docker image that will be used.

1. Ensure that you are in the **root directory** of the project and open a terminal.
2. Run `docker-compose build` to build the image.
   - This may take some time!
3. Run `docker-compose up -d` to spin it up and attach to a container.
4. The container is ready for development.

### Docker Commands

These commands are useful for working with the container but must be run from the root directory of the project.

| Use                    | Command                         |
| ---------------------- | ------------------------------- |
| Start Container        | `docker-compose start`          |
| Stop Container         | `docker-compose stop`           |
| Access Dev Container   | `docker exec -it 3170-env bash` |
| Access Mongo Container | `docker exec -it mongo bash`    |

## Running Project

These provide steps to run the project.

1. In the root directory of the project start the container.
2. Once it has started enter the `3170-env` container, this should take you into the `\commitment` directory.
3. Run `npm install` to install required dependencies, this may take some time.
4. Run `meteor` to run the application.
   - This will be mapped to your `localhost` on `PORT 3000`.

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
