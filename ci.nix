{ sources ? import ./nix/sources.nix
, pkgs ? import ./nix/pkgs.nix { inherit sources; }
}:
let
  versions = {
    "nixos-21_11" = "5a2e2471e8163da8e6f2c1dfd50ef9063199c08b";
  };


  mkReleaseForVersion = version: rev:
    let
      pkgsf = import (builtins.fetchGit {
        url = "https://github.com/NixOS/nixpkgs";
        inherit rev;
      });
      p = import ./nix/pkgs.nix { inherit pkgsf; };
    in
    p.safeColouredTextRelease.overrideAttrs (old: { name = "safe-coloured-text-release-${version}"; });
in
{
  release = pkgs.safeColouredTextRelease;
  pre-commit-check = (import ./nix/pre-commit.nix { inherit sources; }).check;
} // builtins.mapAttrs mkReleaseForVersion versions 
