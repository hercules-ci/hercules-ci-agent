{ mkDerivation, ansi-terminal, async, base, bytestring
, concurrent-output, containers, directory, exceptions, fail
, lifted-async, mmorph, monad-control, mtl, pretty-show, primitive
, random, resourcet, semigroups, stdenv, stm, template-haskell
, text, time, transformers, transformers-base, wl-pprint-annotated
}:
mkDerivation {
  pname = "hedgehog";
  version = "1.0";
  sha256 = "dad69b44b7e0a638fc7f2a9ba2e6a01a3ccd495bbda5c81c5513888ecaebe9da";
  revision = "1";
  editedCabalFile = "1b2hvd3z5mnwfdp5xwdwrh2f1l0wshnnz6ggak2dqqm57wyifdql";
  libraryHaskellDepends = [
    ansi-terminal async base bytestring concurrent-output containers
    directory exceptions fail lifted-async mmorph monad-control mtl
    pretty-show primitive random resourcet semigroups stm
    template-haskell text time transformers transformers-base
    wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers mmorph mtl pretty-show semigroups text transformers
  ];
  homepage = "https://hedgehog.qa";
  description = "Release with confidence";
  license = stdenv.lib.licenses.bsd3;
}
