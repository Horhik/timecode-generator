let
  compilerVersion = "ghc8107";
  compilerSet = pkgs.haskell.packages."${compilerVersion}";


  pkgs = import <nixpkgs> { inherit config; };
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;
  
  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          haskell-nix = super.callCabal2nix "timecode-generator" (gitIgnore [./.gitignore] ./.) {};
        };
      };
    };
  };
  
in {
  inherit pkgs;
  shell = compilerSet.shellFor {
    packages = p: [p.haskell-nix];
    buildInputs = with pkgs; [
      compilerSet.cabal-install
      haskellPackages.tagsoup
      haskellPackages.HTTP
      haskellPackages.zlib
      zlib
    ];
  };
}

