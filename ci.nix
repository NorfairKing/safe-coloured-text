{ sources ? import ./nix/sources.nix
, pkgs ? import ./nix/pkgs.nix { inherit sources; }
}:
let
  versions = {
    "nixos-21_05" = sources.nixpkgs-21_05;
    "nixos-21_11" = sources.nixpkgs-21_11;
    "nixos-22_05" = sources.nixpkgs-22_05;

  };


  mkReleaseForVersion = version: nixpkgs:
    let
      p = import ./nix/pkgs.nix {
        inherit sources nixpkgs;
      };
    in
    p.safeColouredTextRelease.overrideAttrs (old: {
      name = "safe-coloured-text-release-${version}";
    });
in
{
  release = pkgs.safeColouredTextRelease;
  pre-commit-check = (import ./nix/pre-commit.nix { inherit sources; }).check;
  hoogle = pkgs.buildEnv {
    name = "hoogle";
    paths = [
      (pkgs.haskellPackages.ghcWithHoogle (_: pkgs.lib.attrValues pkgs.safeColouredTextPackages))
    ];
  };
} // builtins.mapAttrs mkReleaseForVersion versions
