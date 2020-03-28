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
      prettyprinter = hself.prettyprinter_1_6_0;

      dhall =
        super.lib.flip hlib.addBuildDepends
          [
            hself.atomic-write
            hself.data-fix
            hself.either
            hself.pretty-simple
            hself.prettyprinter
            hself.th-lift-instances
          ]
          (hlib.overrideCabal hsuper.dhall (old:
            let version = "1.30.0";
            in {
              inherit version;
              src = super.fetchurl {
                url = "mirror://hackage/${old.pname}-${version}.tar.gz";
                sha256 = "10aagimwa5ycrq15240ff2g7r0n995waa33vaz0h51nqvncrbgpj";
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
