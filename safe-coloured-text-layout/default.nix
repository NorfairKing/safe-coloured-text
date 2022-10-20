{ mkDerivation, base, bytestring, lib, safe-coloured-text, sydtest
, sydtest-discover, text, validity
}:
mkDerivation {
  pname = "safe-coloured-text-layout";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base safe-coloured-text text validity ];
  testHaskellDepends = [
    base bytestring safe-coloured-text sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/safe-coloured-text#readme";
  description = "Safely layout output coloured text";
  license = lib.licenses.mit;
}
