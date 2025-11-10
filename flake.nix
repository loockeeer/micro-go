{
	description = "Projet de compilation";

	inputs = {
		nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
	};

	outputs = { self, nixpkgs }:
	let
		system = "x86_64-linux";
		pkgs = import nixpkgs { system = system; };
	in
	{
		devShells.${system}.default = pkgs.mkShell {
			packages = with pkgs; [
				ocaml
				dune_2
				ocamlPackages.findlib
				ocamlformat
			];
		};
	};
}

