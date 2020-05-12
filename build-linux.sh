# Build an executable for linux
mkdir build
docker build --tag nix-versions .
docker run -v $(pwd):/mnt nix-versions cp .stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/nix-package-versions-exe/nix-package-versions-exe /mnt/build/nix-package-versions-exe-linux
