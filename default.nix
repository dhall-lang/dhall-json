{ mkDerivation, aeson, aeson-pretty, base, bytestring, dhall
, optparse-generic, stdenv, text, trifecta, unordered-containers
, yaml
}:
mkDerivation {
  pname = "dhall-json";
  version = "1.0.13";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring dhall text trifecta unordered-containers
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring dhall optparse-generic text yaml
  ];
  description = "Compile Dhall to JSON or YAML";
  license = stdenv.lib.licenses.bsd3;
}
