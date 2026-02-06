{
  description = "Perplexity CLI ripper script";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable"; 
  };

  outputs = { self , nixpkgs, ... }: 
  let
    system = "x86_64-linux";
    libraries = with pkgs;[]; 
    pkgs = import nixpkgs { inherit system; };
    pyPkgs = pkgs.python313Packages; 
  in 
  {
    devShells."${system}".default = pkgs.mkShell {
      packages = [
        pkgs.python314
        pyPkgs.selenium
        pyPkgs.python-lsp-server
        pyPkgs.undetected-chromedriver
        pkgs.chromium
      ];

      shellHook = ''
        echo "${pkgs.undetected-chromedriver}" > "driver-path.txt"
      '';
    };
  };
}
