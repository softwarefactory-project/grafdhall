{
  inputs = { nixpkgs.url = "github:NixOS/nixpkgs"; };

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { localSystem = "x86_64-linux"; };
      pkg = pkgs.haskellPackages.callCabal2nix "grafdhall" self { };
    in {
      defaultExe."x86_64-linux" = pkgs.haskell.lib.justStaticExecutables pkg;
      defaultPackage."x86_64-linux" = pkg;
      devShell."x86_64-linux" = pkgs.haskellPackages.shellFor {
        packages = p: [ pkg ];
        buildInputs = [ pkgs.cabal-install ];
      };
    };
}
