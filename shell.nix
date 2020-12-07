{ nixpkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/dd1b7e377f6d77ddee4ab84be11173d3566d6a18.tar.gz") {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = import ./release.nix { pkgs = nixpkgs; };
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.ghcide
    haskellPackages.hlint
    pkgs.ormolu
    pkgs.zlib
  ];
}
