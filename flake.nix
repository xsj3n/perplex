{
  description = "Perplexity CLI ripper";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable"; 
  };

  outputs = { nixpkgs, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    pyPkgs = pkgs.python314Packages;
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
      ];

      shellHook = ''
        echo "${pkgs.undetected-chromedriver}" > "driver-path.txt"
      '';
    };

    packages."${system}".default = pkgs.dockerTools.buildLayeredImage {
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
}
