{
  description = "Projet de compilation";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
  };

  outputs =
    { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { system = system; };
    in
    {
      formatter.${system} = nixpkgs.legacyPackages.${system}.nixfmt-tree;
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          ocaml
          dune_2
          ocamlPackages.findlib
          ocamlformat
        ];
      };
      packages.${system}.default = pkgs.ocamlPackages.buildDunePackage {
        pname = "micro-go";
        version = "1.0.0";

        minimalOCamlVersion = "4.02";

        src = ./.;

        meta = {
          homepage = "https://github.com/loockeeer/micro-go";
          description = "Class project";
        };
      };
    };
}
