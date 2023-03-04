# Build an executable for linux
mkdir build
docker build --tag nix-versions .
docker run -v $(pwd):/mnt nix-versions \
  cp dist-newstyle/build/aarch64-osx/ghc-9.2.5/nix-package-versions-0.1.0.0/x/nix-package-versions-exe/build/nix-package-versions-exe/nix-package-versions-exe /mnt/build/nix-package-versions-exe-linux

