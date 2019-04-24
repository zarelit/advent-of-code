{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base16-bytestring, bytestring
      , containers, cryptohash-md5, directory, hspec, hspec-discover
      , regex-pcre, stdenv
      }:
      mkDerivation {
        pname = "advent-of-code";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base base16-bytestring bytestring containers cryptohash-md5
          directory regex-pcre
        ];
        executableHaskellDepends = [
          base base16-bytestring bytestring containers cryptohash-md5
          directory regex-pcre
        ];
        testHaskellDepends = [
          base base16-bytestring bytestring containers cryptohash-md5
          directory hspec regex-pcre
        ];
        testToolDepends = [ hspec-discover ];
        homepage = "https://github.com/zarelit/advent-of-code";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
