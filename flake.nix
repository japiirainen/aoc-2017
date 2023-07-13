{
  description = "Advent Of Code 2017 in Agda";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs
    , flake-utils
    , self
    , ...
    }:

    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      agda-stdlib = ps: (ps.standard-library.overrideAttrs (_: {
        version = "2.0";
        src = pkgs.fetchFromGitHub {
          repo = "agda-stdlib";
          owner = "agda";
          rev = "177dc9e983606b653a3c6af2ae2162bbc87882ad";
          sha256 = "sha256-ovnhL5otoaACpqHZnk/ucivwtEfBQtGRu4/xw4+Ws+c=";
        };
      }));
      agda = pkgs.agda.withPackages (ps: [ (agda-stdlib ps) ]);
    in
    {
      devShells.default = pkgs.mkShell {
        packages = (with pkgs; [ ghc ]) ++
          (with pkgs.haskellPackages; [ cabal-install agda2hs ]) ++
          [ agda ];

        shellHook = with pkgs;
          ''
            echo "-------------------ENV version info -----------------------"
            ${agda}/bin/agda --version
            ${ghc}/bin/ghc --version
            ${cabal-install}/bin/cabal --version
            echo "------------------------------------------------------------"
          '';
      };

      packages.default = pkgs.stdenv.mkDerivation {
        inherit system;
        name = "aoc";
        src = self;
        buildPhase = ''
          ${agda}/bin/agda -c src/Main.agda --ghc-flag="-o" --ghc-flag="aoc" --compile-dir=_build 
        '';
        installPhase = ''
          mkdir -p $out/bin;
          cp aoc $out/bin;
        '';
      };
    });
}
