{
  description = "Perplexity CLI ripper";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-cabal31420.url = "github:nixos/nixpkgs/f4b140d5b253f5e2a1ff4e5506edbf8267724bde";
  };

  outputs = { nixpkgs, nixpkgs-cabal31420, ... }:
  let
    system = "x86_64-linux";
    haskell_src = ./.; 
    name = "perplex-client";
    pkgs = import nixpkgs { inherit system; };
    c31420 = import nixpkgs-cabal31420 { inherit system; };
    pyPkgs = pkgs.python314Packages;
    hkPkgs = pkgs.haskellPackages;
    pyEnv = pkgs.python314.withPackages (p: with p; [
      selenium
      undetected-chromedriver
    ]);
  in 
  {
    devShells."${system}".default = pkgs.mkShell {
      packages = [
        pkgs.python314
        pyPkgs.selenium
        pyPkgs.python-lsp-server
        pyPkgs.undetected-chromedriver
        pkgs.chromium
        hkPkgs.haskell-language-server
        c31420.cabal-install
        pkgs.zlib
        pkgs.haskell.compiler.native-bignum.ghcHEAD
        hkPkgs.hoogle
        pkgs.xvfb-run
      ];

      shellHook = ''
        echo "${pkgs.undetected-chromedriver}" > "driver-path.txt"
      '';
    };

    packages."${system}" = {

      default = hkPkgs.callCabal2nix "${name}" haskell_src { };

      ct = pkgs.dockerTools.buildLayeredImage {
        name = "perplex-ct";
        tag = "latest";
        contents = with pkgs; [
          chromium
          xorg.libX11
          xorg.libXext
          xorg.libXrender
          fontconfig
          freetype
          glib
          nss
          xvfb-run
          cacert
          dbus
          pyEnv
          (pkgs.writeTextFile {
            name = "script.py";
            destination ="/app/script.py";
            text = builtins.readFile ./main.py;
          })
        ];

        extraCommands = ''
          mkdir -p tmp/
          chmod 1777 tmp/ 
        '';
      
        config = {
          Env = [ "DISPLAY=:99" ];
          Cmd = [
            "xvfb-run"
            "--server-args=-screen 0 1920x1080x24"
            "${pyEnv}/bin/python"
            "/app/script.py"
          ];
        };
      };
    };
  };
}
