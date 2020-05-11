self: super:

let
  lib = super.lib;
  src = super.nix-gitignore.gitignoreSource [] ./.;
  generatedPkg = { hpkgs, mkDerivation ? hpkgs.mkDerivation }:
    let mkDerivation' = args: mkDerivation (args // { inherit src; });
    in (hpkgs.callPackage ./dhall-flycheck.nix { mkDerivation = mkDerivation'; });

  pkg = hpkgs:
    (generatedPkg { inherit hpkgs; }).overrideAttrs (old: {
      passthru.env = (hpkgs.mkDerivation
        (let data = removeAttrs (generatedPkg {
            inherit hpkgs;
            mkDerivation = lib.id;
          }) [ "override" "overrideDerivation" "overrideScope" ] ;
         in data // {
          pname = "pkg-env";
          src = "/dev/null";
          version = "none";
          license = "none";
          buildTools = with hpkgs;
          [
            ghcid
            cabal-install
            hpack
            hscolour
            (hoogleLocal {
              # TODO: all depends
              packages =
                data.libraryHaskellDepends
                ++ data.executableHaskellDepends;
            })
          ];
        })).env;
    });


  hlib = super.haskell.lib;
  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: {
      dhall =
        super.lib.flip hlib.addBuildDepends
          [
            hself.atomic-write
            hself.data-fix
            hself.either
            hself.pretty-simple
            hself.prettyprinter
            hself.th-lift-instances
            hself.text-manipulate
          ]
          (hlib.overrideCabal hsuper.dhall (old:
            let version = "1.32.0";
            in {
              inherit version;
              src = super.fetchurl {
                url = "mirror://hackage/${old.pname}-${version}.tar.gz";
                sha256 = "1imj0bh5365pdizvjbw2wqz0g9hakigf1zm4fr6379qdchxpp90p";
              };
              editedCabalFile = null;
          }));

        dhall-flycheck = pkg hself;
    };
  };

  dhall-flycheck =
    hlib.justStaticExecutables haskellPackages.dhall-flycheck;

in {
  inherit
    dhall-flycheck;
}
