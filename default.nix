with import <nixpkgs> {}; {
  minerEnv = stdenv.mkDerivation {
    name = "miner";
    buildInputs = [ fuse (haskellPackages.ghcWithPackages (haskellPackages: with haskellPackages; [ HFuse parsec dataenc auto-update ])) ];
  };
}
