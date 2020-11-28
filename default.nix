{ mkDerivation, base, hedgehog, hpack, hspec, process, regex-tdfa
, stdenv, tasty, tasty-hedgehog, tasty-hspec
}:
mkDerivation {
  pname = "geeklets-hs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base process regex-tdfa ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base process regex-tdfa ];
  testHaskellDepends = [
    base hedgehog hspec process regex-tdfa tasty tasty-hedgehog
    tasty-hspec
  ];
  prePatch = "hpack";
  homepage = "https://github.com/tbidne/geeklets-hs#readme";
  license = stdenv.lib.licenses.bsd3;
}
