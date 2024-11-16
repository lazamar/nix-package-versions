#!/usr/bin/env bash
set -e

function build {
  cabal v2-build -j nix-package-versions-exe
}

function watch-exe {
  ghcid -c 'cabal v2-repl' nix-package-versions-exe
}

function watch-lib {
  ghcid -c 'cabal v2-repl' nix-package-versions
}

function watch-test {
  ghcid -c 'cabal v2-repl' nix-package-versions-test
}

function test {
  cabal v2-run nix-package-versions-test
}

function server {
  cabal v2-run nix-package-versions-exe -- \
    server \
    --port 8080 \
    --db-root database
}

function update-database {
  # Load GitHub credentials
  source $(pwd)/.secrets || echo 'warning: no .secrets file' # allow setting env in a different way

  echo user: $GITHUB_USER
  echo token: $GITHUB_TOKEN

  cabal v2-run nix-package-versions-exe -- \
    update \
    --from  2024-01-01 \
    --github-user  $GITHUB_USER \
    --github-token $GITHUB_TOKEN \
    --db-root database \
    +RTS -N
}

# If the first argument is a function run it.
if [[ $(type -t $1) == function ]];
  then
   $1 "${@:2}";
  else
    echo "Development utilities"
    echo "To perform commands run:"
    echo ""
    echo "  ./utils.sh COMMAND"
    echo ""
fi
