FROM haskell:8.6.5

# Set root password to 'root'
RUN echo 'root:root' |  chpasswd

RUN apt-get update && apt-get install -y curl bzip2 adduser tmux vim

# Build project as root
COPY . /home/marcelo
WORKDIR /home/marcelo
RUN stack build

# Nix installation
RUN adduser --disabled-password --gecos '' marcelo
# Nix requires ownership of /nix.
RUN mkdir -m 0755 /nix && chown marcelo /nix
RUN chmod 0755 /home/marcelo && chown -R marcelo /home/marcelo
# Change docker user to marcelo
USER marcelo
# Set some environment variables for Docker and Nix
ENV USER marcelo
# Change our working directory to $HOME
WORKDIR /home/marcelo
# install Nix
RUN curl https://nixos.org/nix/install | sh
# update the nix channels
# Note: nix.sh sets some environment variables. Unfortunately in Docker
# environment variables don't persist across `RUN` commands
# without using Docker's own `ENV` command, so we need to prefix
# our nix commands with `. .nix-profile/etc/profile.d/nix.sh` to ensure
# nix manages our $PATH appropriately.
RUN . .nix-profile/etc/profile.d/nix.sh && nix-channel --update

