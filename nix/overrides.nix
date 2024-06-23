{ lib, haskell, symlinkJoin, ... }:
with lib;
with haskell.lib;
self: super:
let
  safeColouredTextPkg = name:
    doBenchmark (buildStrictly (self.callPackage (../${name}) { }));
  safeColouredTextPackages =
    genAttrs [
      "safe-coloured-text"
      "safe-coloured-text-gen"
      "safe-coloured-text-layout"
      "safe-coloured-text-layout-gen"
      "safe-coloured-text-terminfo"

    ]
      safeColouredTextPkg;
in
{
  inherit safeColouredTextPackages;

  safeColouredTextRelease = symlinkJoin {
    name = "safe-coloured-text-release";
    paths = attrValues self.safeColouredTextPackages;
    passthru = self.safeColouredTextPackages;
  };
} // safeColouredTextPackages
