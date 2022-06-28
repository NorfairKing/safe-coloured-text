final: previous:
with final.haskell.lib;

{
  safeColouredTextPackages =
    let
      safeColouredTextPkg =
        name:
        buildStrictly (
          overrideCabal (final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { })
            (old: {
              doBenchmark = true;
              configureFlags = (old.configureFlags or [ ]) ++ [
                # Optimisations
                "--ghc-options=-O2"
                # Extra warnings
                "--ghc-options=-Wall"
                "--ghc-options=-Wincomplete-uni-patterns"
                "--ghc-options=-Wincomplete-record-updates"
                "--ghc-options=-Wpartial-fields"
                "--ghc-options=-Widentities"
                "--ghc-options=-Wredundant-constraints"
                "--ghc-options=-Wcpp-undef"
                "--ghc-options=-Werror"
                "--ghc-options=-Wno-deprecations"
              ];
              # Ugly hack because we can't just add flags to the 'test' invocation.
              # Show test output as we go, instead of all at once afterwards.
              testTarget = (old.testTarget or "") + " --show-details=direct";
            })
        );
    in
    {
      "safe-coloured-text" = safeColouredTextPkg "safe-coloured-text";
      "safe-coloured-text-gen" = safeColouredTextPkg "safe-coloured-text-gen";
      "safe-coloured-text-layout" = safeColouredTextPkg "safe-coloured-text-layout";
      "safe-coloured-text-layout-gen" = safeColouredTextPkg "safe-coloured-text-layout-gen";
      "safe-coloured-text-terminfo" = safeColouredTextPkg "safe-coloured-text-terminfo";
    };

  safeColouredTextRelease =
    final.symlinkJoin {
      name = "safe-coloured-text-release";
      paths = final.lib.attrValues final.safeColouredTextPackages;
    };

  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super:
                let
                  # envparse
                  envparseRepo =
                    final.fetchFromGitHub {
                      owner = "supki";
                      repo = "envparse";
                      rev = "de5944fb09e9d941fafa35c0f05446af348e7b4d";
                      sha256 =
                        "sha256:0piljyzplj3bjylnxqfl4zpc3vc88i9fjhsj06bk7xj48dv3jg3b";
                    };
                  envparsePkg =
                    dontCheck (
                      self.callCabal2nix "envparse" envparseRepo { }
                    );

                in
                final.safeColouredTextPackages // {
                  envparse = envparsePkg;
                }
            );
      }
    );
}
