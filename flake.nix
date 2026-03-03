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
    hkPkgs = pkgs.haskellPackages;
    pyEnv = pkgs.python314.withPackages (p: [
      p.selenium
      p.python-lsp-server
      p.undetected-chromedriver
      
    ]);
    hkInputs = [
      hkPkgs.haskell-language-server
      c31420.cabal-install
      pkgs.zlib
      pkgs.haskell.compiler.native-bignum.ghcHEAD
    ];

    otherInputs = [
      pkgs.chromium
      pkgs.xvfb-run
      pkgs.undetected-chromedriver
    ];
    
    
    client = hkPkgs.callCabal2nix "${name}" src { };

    mainPy = pkgs.replaceVars ./main.py {
      driverPath = "${pkgs.undetected-chromedriver}";
      chromePath = "${pkgs.chromium}";
    };  
  
    server = pkgs.stdenv.mkDerivation {
      name = "perplex-selenium-server";
      src = src;
      buildInputs = otherInputs ++ [ pyEnv ];
      dontUnpack = true;
      installPhase = ''
        mkdir -p $out/bin $out/share/server/
        cp ${mainPy}  $out/share/server/
      '';
    };
  in 
  {
    devShells."${system}".default = pkgs.mkShell {
      packages = hkInputs ++ [ pyEnv.pkgs ];
    };

    packages."${system}".default = pkgs.symlinkJoin {
        name = "perplex";
        paths = [ server ];
        buildInputs = [ pkgs.bash pkgs.xvfb-run client server ];
        postBuild = ''
          cat > $out/bin/${name} <<'EOF'
          #!/usr/bin/env sh
          ${pkgs.xvfb-run}/bin/xvfb-run ${pyEnv}/bin/python -u ${mainPy} &
          exec ${client}/bin/${name}
          EOF
          chmod +x $out/bin/${name}
        '';
    };

    apps."${system}".default = {
      type = "app";
      program = "${self.packages.${system}.default}/bin/${name}";
      
    };
  };
}
