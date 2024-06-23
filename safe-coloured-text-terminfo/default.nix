{ mkDerivation, base, lib, safe-coloured-text, terminfo }:
mkDerivation {
  pname = "safe-coloured-text-terminfo";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [ base safe-coloured-text terminfo ];
  homepage = "https://github.com/NorfairKing/safe-coloured-text#readme";
  description = "Safely output coloured text";
  license = lib.licenses.mit;
}
