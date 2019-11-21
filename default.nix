{ pkgs ? import ./nix/pkgs.nix {} }:
[
    pkgs.gnumake
    pkgs.python3Packages.sphinx
]
