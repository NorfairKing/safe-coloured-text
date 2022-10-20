{ mkDerivation, base, genvalidity, genvalidity-sydtest, lib
, safe-coloured-text, safe-coloured-text-gen
, safe-coloured-text-layout, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "safe-coloured-text-layout-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity safe-coloured-text-gen safe-coloured-text-layout
  ];
  testHaskellDepends = [
    base genvalidity-sydtest safe-coloured-text safe-coloured-text-gen
    safe-coloured-text-layout sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/safe-coloured-text#readme";
  license = lib.licenses.mit;
}
