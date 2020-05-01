# Notes

## Pinning Nix Pkgs versinos
https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs


    let
        revision = builtins.fetchGit {
            # Descriptive name to make the store path easier to identify
            name = "does-the-name-matter";
            url = "https://github.com/nixos/nixpkgs-channels/";
            # Commit hash for nixos-unstable as of 2018-09-12
            # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
            ref = "refs/heads/nixpkgs-17.09-darwin";
            rev = "24a7883c2349af5076107dbbb615be09d6025a95";
          };
    in with import revision {};
    (import <nixpkgs> {}).mkShell {
        buildInputs = [
            vim
        ];
    }

## On cache busting

Find the version of nixpkgs with the package version you want and pin nixpkgs to that. However, be aware that the pinning of a package of another nixpkgs version results in a much larger package size as not only the package itself but all dependencies (down to libc) have older versions.

## Implementation notes:

The commit route is not fruitful. Packages will point to different files for different versions.
Also there are far too many packages definied in a multi-definition file

# List all packages of a derivation

Get json list of all packages for a nix revision

    $GIT_HASH=98d8e1a160d3138e9ef3a51f727b04315ab9e285
    nix-env -qaP --json -f https://github.com/NixOS/nixpkgs-channels/archive/$GIT_HASH.tar.gz --arg config 'import ./packages-config.nix'

# Install a package from a revision number

    nix-env -qaP ghc -f https://github.com/NixOS/nixpkgs-channels/archive/03f76c136de2a907923ec66a91b79e058a2aad7d.tar.gz

# Try it in nix-shell

    nix-shell -p bat -I nixpkgs=https://github.com/nixos/nixpkgs-channels/archive/6ea7b48c2b20ab88334cea9b368fe0d0354b4926.tar.gz

## Statistics about Nix

Total number of packages loaded: 60895
Packages from files with one definition: 9623
Packages from files with multiple definition: 51189
Packages without definition path: 83
Paths with most packages
("pkgs/development/r-modules/generic-builder.nix",18287)
("pkgs/development/haskell-modules/hackage-packages.nix",14970)
("lib/attrsets.nix",2267)
("pkgs/top-level/ruby-packages.nix",1012)
("pkgs/development/perl-modules/generic/default.nix",827)
("pkgs/misc/vim-plugins/generated.nix",544)
("pkgs/development/lua-modules/generated-packages.nix",304)
("pkgs/development/lisp-modules/define-package.nix",236)
("pkgs/servers/x11/xorg/default.nix",219)
("pkgs/top-level/php-packages.nix",196)
("pkgs/build-support/trivial-builders.nix",166)
("pkgs/development/libraries/kde-frameworks/default.nix",148)
("pkgs/applications/kde/default.nix",114)
("pkgs/applications/networking/cluster/terraform-providers/default.nix",104)
("pkgs/development/libraries/aspell/dictionaries.nix",93)
("pkgs/development/libraries/hunspell/dictionaries.nix",84)
("pkgs/development/ocaml-modules/janestreet/janePackage_0_12.nix",69)
("pkgs/os-specific/linux/nvidia-x11/generic.nix",68)
("pkgs/top-level/dotnet-packages.nix",61)
("pkgs/misc/emulators/retroarch/cores.nix",59)
("pkgs/desktops/plasma-5/default.nix",45)
("pkgs/os-specific/linux/zfs/default.nix",36)
("pkgs/development/coq-modules/mathcomp/default.nix",36)
("pkgs/build-support/cc-wrapper/default.nix",33)
("pkgs/misc/uboot/default.nix",31)
("pkgs/development/libraries/qt-5/qtModule.nix",28)
("pkgs/development/coq-modules/mathcomp/extra.nix",25)
("pkgs/misc/tmux-plugins/default.nix",25)
("pkgs/os-specific/linux/kernel/manual-config.nix",25)
("pkgs/applications/science/machine-learning/torch/torch-distro.nix",23)


# Database performance investigation

I removed revision a3962299f14944a0e9ccf8fd84bd7be524b74cd6 from a database of 1.1Gb and I'm running re-inserts to see what
effect it is having.

Inserting 42K records with my packages + revisions schema that does not require joins for searching I have:

 - 53.01s saving to database when records are not there (no concurrency)
 - 22.16s saving to database when records are there
 - data not there with 5 threads one thread takes 53 seconds and all others take 22 seconds. Total of 145 seconds.
 - data already there with 5 threads all take around 20 seconds. Total of 104 seconds.
 - fresh database 47 seconds for insert and 18 seconds for noop inserts. Total 123 seconds.

