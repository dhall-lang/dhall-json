{ mkDerivation, aeson, base, bytestring, dhall, neat-interpolation
, optparse-generic, stdenv, text, trifecta, vector, yaml
}:
mkDerivation {
  pname = "dhall-json";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base dhall neat-interpolation text vector
  ];
  executableHaskellDepends = [
    aeson base bytestring dhall optparse-generic text trifecta yaml
  ];
  description = "Dhall to JSON compiler";
  license = stdenv.lib.licenses.bsd3;
}
