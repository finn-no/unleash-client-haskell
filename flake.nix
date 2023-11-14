{
  description = "unleash-client-haskell";
  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs, flake-compat, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        unleash-client-haskell = pkgs.haskellPackages.callCabal2nix "unleash-client-haskell" ./. { };
      in {
        defaultPackage = unleash-client-haskell;
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            cabal-install
            haskellPackages.fourmolu
            haskell-language-server
            hlint
            haskellPackages.implicit-hie
            nixfmt
            nodePackages.conventional-changelog-cli
          ];
          inputsFrom = [ unleash-client-haskell.env ];
        };
      });
}
