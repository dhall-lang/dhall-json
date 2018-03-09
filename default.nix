{ mkDerivation, aeson, aeson-pretty, base, bytestring, dhall
, optparse-generic, stdenv, text, trifecta, yaml
}:
mkDerivation {
  pname = "dhall-json";
  version = "1.0.12";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base dhall text ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring dhall optparse-generic text
    trifecta yaml
  ];
  description = "Compile Dhall to JSON or YAML";
  license = stdenv.lib.licenses.bsd3;
}
