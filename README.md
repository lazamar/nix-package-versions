# [Nix Package Versions](https://lazamar.co.uk/nix-versions/)

[![GitHub CI](https://github.com/lazamar/nix-package-versions/workflows/CI/badge.svg)](https://github.com/lazamar/nix-package-versions/actions)

This project aims to make it easy to find old versions of packages to install using the [Nix package manager](https://nixos.org/nix/).
It does that by allowing you to search a package name and finding all past versions of that package that were available in the Nix
package repository and listing the latest revision at which they were available.

[Go to search](https://lazamar.co.uk/nix-versions/)

## Motivation and Method

Installing older versions of packages with Nix is easy but currently there is no official way to find out what
revision has the package version that I need. This project provides this functionality by letting you see what
versions were available in the past, when they were available, what revision to install them from, and what command to use.

The list of versions available here is not exhaustive. To check what versions of packages were ever available data
was collected from past revisions in 5 weeks intervals. This means that if a package changed versions more often
than every 5 weeks there may be versions missing.

During the retrieval of version information, revisions for some periods could not be successfully built for
some channels and this could also cause versions to be missing from the list.
