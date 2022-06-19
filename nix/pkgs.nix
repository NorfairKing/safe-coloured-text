{ sources ? import ./sources.nix
, nixpkgs ? sources.nixpkgs
}:
let
  safeColouredTextPkgs =
    import nixpkgs {
      overlays =
        [
          (import (sources.autodocodec + "/nix/overlay.nix"))
          (import (sources.validity + "/nix/overlay.nix"))
          (import (sources.sydtest + "/nix/overlay.nix"))
          (final: previous: { niv = (import sources.niv { pkgs = final; }).niv; })
          (import ./gitignore-src.nix)
          (import ./overlay.nix)
        ];
      config.allowUnfree = true;
    };
in
safeColouredTextPkgs
