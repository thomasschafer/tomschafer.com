{
  description = "Personal website development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Haskell build tools
            ghc
            stack
            cabal-install

            # C libraries needed by Haskell packages
            zlib
            zlib.dev
            pkg-config

            # Frontend tools
            sass
            netlify-cli

            # Other tools
            gnumake
            git
          ];

          shellHook = ''
            if [ -f .env ]; then
              set -o allexport
              source .env
              set +o allexport
            fi
          '';
        };
      }
    );
}
