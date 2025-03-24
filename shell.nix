{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {
  buildInputs = with pkgs; [
    # Haskell build tools
    ghc
    stack
    cabal-install

    # C libraries needed by Haskell packages
    zlib
    zlib.dev
    pkg-config

    # For Sass compilation
    sass

    # For deployment
    nodePackages.netlify-cli

    # Build tools
    gnumake
  ];

  shellHook = ''
    # Load environment variables from .env file
    if [ -f .env ]; then
      set -o allexport
      source .env
      set +o allexport
    fi

    # Add LD_LIBRARY_PATH for zlib
    export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs.zlib ]}:$LD_LIBRARY_PATH
  '';
}
