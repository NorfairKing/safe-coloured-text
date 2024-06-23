{ mkDerivation, base, bytestring, lib, text, validity
, validity-bytestring, validity-text
}:
mkDerivation {
  pname = "safe-coloured-text";
  version = "0.3.0.2";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring text validity validity-bytestring validity-text
  ];
  homepage = "https://github.com/NorfairKing/safe-coloured-text#readme";
  description = "Safely output coloured text";
  license = lib.licenses.mit;
}
