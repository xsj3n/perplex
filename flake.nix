{
  description = "Perplexity CLI ripper";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-cabal31420.url = "github:nixos/nixpkgs/f4b140d5b253f5e2a1ff4e5506edbf8267724bde";
  };

  outputs = { self, nixpkgs, nixpkgs-cabal31420, ... }:
  let
    system = "x86_64-linux";
    src = ./.; 
    name = "perplex-client";
    pkgs = import nixpkgs { inherit system; };
    c31420 = import nixpkgs-cabal31420 { inherit system; };
    pyPkgs = pkgs.python314Packages;
    hkPkgs = pkgs.haskellPackages;
    pyInputs = [
      pkgs.python314
      pyPkgs.selenium
      pyPkgs.python-lsp-server
      pyPkgs.undetected-chromedriver
      pkgs.chromium
      pkgs.xvfb-run
    ];
    hkInputs = [
      hkPkgs.haskell-language-server
      c31420.cabal-install
      pkgs.zlib
      pkgs.haskell.compiler.native-bignum.ghcHEAD
    ];

    client = hkPkgs.callCabal2nix "${name}" src { };

    server = pkgs.stdenv.mkDerivation {
      name = "perplex-selenium-server";
      src = src;
      buildInputs = pyInputs;
      dontUnpack = true;
      installPhase = ''
        mkdir -p $out/bin $out/share/server/
        cp ${src}/main.py $out/share/server/
        echo "${pkgs.undetected-chromedriver}" > $out/share/server/driver-path.txt
      '';
    };
  in 
  {
    devShells."${system}".default = pkgs.mkShell {
      packages = hkInputs ++ pyInputs;
      shellHook = ''
        echo "${pkgs.undetected-chromedriver}" > "driver-path.txt"
      '';
    };

    packages."${system}".default = pkgs.symlinkJoin {
        name = "perplex";
        paths = [ server ];
        buildInputs = [ pkgs.bash pkgs.xvfb-run client];
        postBuild = ''
          cat > $out/bin/perplex <<'EOF'
          #!/usr/bin/env sh
          #${pkgs.xvfb-run}/bin/xvfb-run
          python ${server}/share/server/main.py &
          exec ${client}/bin/${name}
          EOF
          chmod +x $out/bin/perplex
        '';
    };

    apps."${system}".default = {
      type = "app";
      program = "${self.packages.${system}.default}/bin/perplex";
    };
  };
}
