{
  description = "A simple haskell ssg";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  outputs =
    { nixpkgs, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forEachSystem = nixpkgs.lib.genAttrs systems;
    in
    {
      devShells = forEachSystem (
        system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs.haskellPackages; [ ghc cabal-install haskell-language-server ];
          };
        }
      );
      apps = forEachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          build = pkgs.writeShellApplication {
            name = "build";
            runtimeInputs = with pkgs.haskellPackages; [ ghc cabal-install ];
            text = ''
              cabal update
              cabal run site-build
            '';
          };
        in
        {
          build = { type = "app"; program = "${build}/bin/build"; };
        }
      );
    };
}
