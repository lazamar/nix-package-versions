{
  description = "Home Manager configuration of JulienS";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs @ {
    nixpkgs,
    flake-utils,
    nixpkgs-master,
    ...
  }:
  {
  }
  // flake-utils.lib.eachDefaultSystem (system: let
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    devShells.default = (pkgs.buildFHSUserEnv {
      name = "default";
      multiPkgs = pkgs: (with pkgs; [ zlib ghcid cabal-install haskell.compiler.ghc925 haskellPackages.zlib-bindings zlib zlib haskellPackages.zlib-bindings  zlib.dev zlib.out haskellPackages.warp]);
    }).env;
  });
}
