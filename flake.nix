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
      "github:finn-no/unleash-client-haskell-core?ref=985e3cdec3a944c5ba5260fef69903b445641690";
  };
  outputs = { self, nixpkgs, flake-compat, flake-utils, unleash-client-haskell-core }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (self: super: {
              haskellPackages = super.haskellPackages.override {
                overrides = self: super: {
                  unleash-client-haskell-core =
                    pkgs.haskell.lib.dontCheck unleash-client-haskell-core.defaultPackage.${system};
                };
              };
            })
          ];
        };
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
