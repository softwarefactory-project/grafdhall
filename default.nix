let
  nixpkgs = import (fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/9816b99e71c3504b0b4c1f8b2e004148460029d4.tar.gz")
    { };
  name = "grafdhall";
  hsPkgs = nixpkgs.haskellPackages.override {
    overrides = self: super: {
      grafana = nixpkgs.haskell.lib.overrideCabal super.grafana {
        version = "v0.3";
        broken = false;
        src = builtins.fetchGit {
          url = "https://github.com/rrruko/grafana.git";
          ref = "master";
          rev = "bf67d3c973849e4d7513c677c9043091f699f293";
        };
      };
    };
  };
  drv = hsPkgs.callCabal2nix name ./. { };
  shellDrv = hsPkgs.shellFor {
    withHoogle = false;
    packages = p: [ drv ];
    buildInputs = with hsPkgs; [ hlint cabal-install haskell-language-server ];
  };
in if nixpkgs.lib.inNixShell then shellDrv else drv
