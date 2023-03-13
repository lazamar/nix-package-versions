FROM haskell:9.2.5

RUN apt-get update && apt-get install -y curl bzip2 adduser tmux vim sqlite3

# Copy project to container
COPY . /home/app
WORKDIR /home/app

# Install Nix
# The sheer existence of this user and group makes the nix installation work.
RUN groupadd -r nixbld
RUN useradd -rMN -s /bin/bash -g nixbld -G nixbld -u 1001 mainuser
RUN mkdir -m 0755 /nix && chown root /nix
RUN curl --insecure -L https://nixos.org/nix/install | sh
ENV USER=root
RUN echo ". $HOME/.nix-profile/etc/profile.d/nix.sh" >> $HOME/.bashrc

# Build project
RUN cabal update && cabal v2-build
