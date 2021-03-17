let
  nixpkgs = import (fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/9816b99e71c3504b0b4c1f8b2e004148460029d4.tar.gz")
    { };
  name = "grafdhall";
  hsPkgs = nixpkgs.haskellPackages;
  drv = hsPkgs.callCabal2nix name ./. { };
  shellDrv = hsPkgs.shellFor {
    withHoogle = false;
    packages = p: [ drv ];
    buildInputs = with hsPkgs; [ hlint cabal-install haskell-language-server ];
  };
in if nixpkgs.lib.inNixShell then shellDrv else drv
