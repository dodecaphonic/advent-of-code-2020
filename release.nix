{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/dd1b7e377f6d77ddee4ab84be11173d3566d6a18.tar.gz") {}}:
  pkgs.haskellPackages.callPackage ./default.nix {}
