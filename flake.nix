{
  description = "safe-coloured-text";
  nixConfig = {
    extra-substituters = "https://safe-coloured-text.cachix.org";
    extra-trusted-public-keys = "safe-coloured-text.cachix.org-1:BriaRVzOr8kxni3hMCw/BRhKx85SkltChBw6PVdt884=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.11";
    nixpkgs-22_05.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    nixpkgs-21_11.url = "github:NixOS/nixpkgs?ref=nixos-21.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    autodocodec.url = "github:NorfairKing/autodocodec?ref=flake";
    autodocodec.flake = false;
    validity.url = "github:NorfairKing/validity?ref=flake";
    validity.flake = false;
    sydtest.url = "github:NorfairKing/sydtest?ref=flake";
    sydtest.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-22_05
    , nixpkgs-21_11
    , flake-utils
    , pre-commit-hooks
    , autodocodec
    , validity
    , sydtest
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.${system}
          (import (validity + "/nix/overlay.nix"))
          (import (autodocodec + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
        ];
      };
      pkgs = pkgsFor nixpkgs;
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.haskellPackages.safeColouredTextRelease;
      checks.${system} =
        let
          backwardCompatibilityCheckFor = nixpkgs:
            let pkgs' = pkgsFor nixpkgs;
            in pkgs'.haskellPackages.safeColouredTextRelease;
          allNixpkgs = {
            inherit
              nixpkgs-22_05
              nixpkgs-21_11;
          };
          backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
        in
        backwardCompatibilityChecks // {
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              hpack.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
              nixpkgs-fmt.excludes = [ ".*/default.nix" ];
              cabal2nix.enable = true;
            };
          };
        };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "safe-coloured-text-shell";
        packages = p: builtins.attrValues p.safeColouredTextPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          niv
          zlib
          cabal-install
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
