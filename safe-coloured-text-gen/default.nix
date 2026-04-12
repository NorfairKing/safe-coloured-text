{ mkDerivation, base, bytestring, genvalidity
, genvalidity-bytestring, genvalidity-sydtest, genvalidity-text
, lib, path, path-io, safe-coloured-text
, safe-coloured-text-parsing, sydtest, sydtest-discover, text
}:
mkDerivation {
  pname = "safe-coloured-text-gen";
  version = "0.0.0.3";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-bytestring genvalidity-text
    safe-coloured-text
  ];
  testHaskellDepends = [
    base bytestring genvalidity-sydtest path path-io safe-coloured-text
    safe-coloured-text-parsing sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/safe-coloured-text#readme";
  license = lib.licenses.mit;
}
