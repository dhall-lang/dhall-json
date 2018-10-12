{ mkDerivation, ansi-terminal, base, bytestring, case-insensitive
, cborg, containers, contravariant, criterion, cryptonite, deepseq
, Diff, directory, doctest, exceptions, fetchgit, filepath
, haskeline, http-client, http-client-tls, lens-family-core
, megaparsec, memory, mockery, mtl, optparse-applicative, parsers
, prettyprinter, prettyprinter-ansi-terminal, QuickCheck
, quickcheck-instances, repline, scientific, serialise, stdenv
, tasty, tasty-hunit, tasty-quickcheck, template-haskell, text
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.17.0";
  src = fetchgit {
    url = "https://github.com/dhall-lang/dhall-haskell.git";
    sha256 = "0pivzmmx7qlkqkan0xzjm0il114lxhvpwwlvnc6fdfjaawvgb5bg";
    rev = "afffa17be44047169cfb335a65ebb63d9d7a7868";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring case-insensitive cborg containers
    contravariant cryptonite Diff directory exceptions filepath
    haskeline http-client http-client-tls lens-family-core megaparsec
    memory mtl optparse-applicative parsers prettyprinter
    prettyprinter-ansi-terminal repline scientific serialise
    template-haskell text transformers unordered-containers vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers deepseq directory doctest filepath mockery
    prettyprinter QuickCheck quickcheck-instances serialise tasty
    tasty-hunit tasty-quickcheck text transformers vector
  ];
  benchmarkHaskellDepends = [
    base bytestring containers criterion directory serialise text
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
