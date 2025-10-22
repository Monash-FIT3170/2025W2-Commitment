# docker build -t python-executor-img ./python_executor/
# docker run --rm -it --cap-add=SYS_ADMIN --cap-add=SYS_CHROOT --security-opt seccomp=unconfined --security-opt apparmor=unconfigured python-executor-img

FROM nixos/nix

# Add dependencies to the environment
RUN nix-env -iA nixpkgs.bash

WORKDIR /home/python

# Copy application files
COPY . .

# Build the nix project
RUN nix-build

EXPOSE 8002

# Keep container interactive for testing
CMD ["/home/python/result/bin/python_executor.py"]
