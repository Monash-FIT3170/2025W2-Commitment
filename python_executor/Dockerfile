# docker build -t python-executor-img ./python_executor/
# docker run --rm -it --cap-add=SYS_ADMIN --cap-add=SYS_CHROOT --security-opt seccomp=unconfined --security-opt apparmor=unconfigured python-executor-img

FROM nixos/nix

RUN #nix-channel --update

# Add dependencies to the environment
RUN nix-env -iA nixpkgs.bubblewrap nixpkgs.bash

WORKDIR /home/python

# Copy application files
COPY . .

# Build your nix project
RUN nix-build

EXPOSE 8000

# Keep container interactive
CMD ["bash"]
