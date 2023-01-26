{
  description = "unleash-client-haskell";
  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    unleash-client-haskell-core.url =
      "github:finn-no/unleash-client-haskell-core?ref=0.8.0";
  };
  outputs =
    { self, nixpkgs, flake-compat, flake-utils, unleash-client-haskell-core }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (self: super: {
              haskellPackages = super.haskellPackages.override {
                overrides = self: super: {
                  unleash-client-haskell-core = pkgs.haskell.lib.dontCheck
                    unleash-client-haskell-core.defaultPackage.${system};
                };
              };
            })
          ];
        };
        unleash-client-haskell =
          pkgs.haskellPackages.callCabal2nix "unleash-client-haskell" ./. { };
      in {
        defaultPackage = unleash-client-haskell; # nix build .?submodules=1
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            cabal-install
            haskellPackages.fourmolu
            haskell-language-server
            hlint
            haskellPackages.implicit-hie
            nixfmt
          ];
          inputsFrom = [ unleash-client-haskell.env ];
        };
      });
}
