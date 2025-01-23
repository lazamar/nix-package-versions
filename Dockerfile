FROM haskell:9.10.1-bullseye

RUN apt-get update && apt-get install -y curl bzip2 adduser tmux vim sqlite3

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
COPY cabal.project nix-package-versions.cabal /home/app/
RUN cabal update && cabal v2-build --only-dependencies
COPY . /home/app
RUN cabal v2-build -j
RUN cabal install

# Add executable to PATH as 'nix-package-versions'
RUN ln -s $(cabal list-bin nix-package-versions-exe) /usr/local/bin/nix-package-versions

ENTRYPOINT ["nix-package-versions"]
