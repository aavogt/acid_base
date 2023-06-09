# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, blazeHtml, cmdargs, dataDefault, digestiveFunctors
, digestiveFunctorsBlaze, digestiveFunctorsHappstack
, happstackServer, hmatrix, hmatrixGsl, lens, mtl, tagsoup, text, trifecta
, xhtml, fetchdarcs
}:

cabal.mkDerivation (self: {
  pname = "acid-base";
  version = "0.1.0.0";
  src = if (builtins.pathExists ./acid_base.cabal)
        then builtins.filterSource (path : type : ! (type == "directory" && baseNameOf (toString path) == "dist")) ./.
        else fetchdarcs {
                url = http://code.haskell.org/~aavogt/acid_base ;
                sha256 = "";
            };
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    blazeHtml cmdargs dataDefault digestiveFunctors
    digestiveFunctorsBlaze digestiveFunctorsHappstack happstackServer
    hmatrix lens mtl tagsoup text trifecta xhtml hmatrixGsl
  ];
  meta = {
    description = "web app that calculates acid-base equilibria";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
