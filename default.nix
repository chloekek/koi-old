{ pkgs ? import ./nix/pkgs.nix {} }:
[
    pkgs.coreutils
    pkgs.gnumake
    pkgs.mercury
    pkgs.python3Packages.sphinx
]
