let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, dconfig ? "testnet_staging"
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; }) }:

with pkgs.lib;
with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let
  addConfigureFlags = flags: drv: overrideCabal drv (drv: {
    configureFlags = flags;
  });
  cleanSource2 = builtins.filterSource (name: type: let
      f1 = cleanSourceFilter name type;
      baseName = baseNameOf (toString name);
      f2 = ! (type == "symlink" && hasSuffix ".root" baseName);
      f3 = ! (hasSuffix ".swp" baseName);
      f4 = ! (baseName == ".stack-work");
      f5 = ! (hasSuffix ".nix" baseName);
    in f1 && f2 && f3 && f4 && f5);
in ((import ./pkgs { inherit pkgs; }).override {
  overrides = self: super: {
    cardano-sl = overrideCabal super.cardano-sl (drv: {
      src = cleanSource2 drv.src;
      doHaddock = false;
      patchPhase = ''
       export CSL_SYSTEM_TAG=linux64
      '';
      # production full nodes shouldn't use wallet as it means different constants
      configureFlags = [
        "-f-asserts"
        "-f-dev-mode"
        "-fwith-explorer"
        # https://github.com/NixOS/nixpkgs/pull/24692#issuecomment-306509337
        "--ghc-option=-optl-lm"
      ];
    });
    cardano-sl-core = overrideCabal super.cardano-sl-core (drv: {
      src = cleanSource2 drv.src;
      configureFlags = [
        "-f-embed-config"
        "-f-asserts"
        "-f-dev-mode"
        "--ghc-options=-DCONFIG=${dconfig}"
      ];
    });
    # TODO: patch cabal2nix to allow this
    cardano-sl-db = overrideCabal super.cardano-sl-db (drv: { src = cleanSource2 drv.src; });
    cardano-sl-infra = overrideCabal super.cardano-sl-infra (drv: { src = cleanSource2 drv.src; });
    cardano-sl-lrc = overrideCabal super.cardano-sl-lrc (drv: { src = cleanSource2 drv.src; });
    cardano-sl-ssc = overrideCabal super.cardano-sl-ssc (drv: { src = cleanSource2 drv.src; });
    cardano-sl-txp = overrideCabal super.cardano-sl-txp (drv: { src = cleanSource2 drv.src; });
    cardano-sl-update = overrideCabal super.cardano-sl-update (drv: { src = cleanSource2 drv.src; });
    cardano-sl-godtossing = overrideCabal super.cardano-sl-godtossing (drv: { src = cleanSource2 drv.src; });
    cardano-sl-lwallet = overrideCabal super.cardano-sl-lwallet (drv: { src = cleanSource2 drv.src; });
    cardano-sl-tools = overrideCabal super.cardano-sl-tools (drv: { src = cleanSource2 drv.src; });

    cardano-sl-static = justStaticExecutables self.cardano-sl;
    # Gold linker fixes
    cryptonite = addConfigureFlags ["--ghc-option=-optl-pthread"] super.cryptonite;
    mkDerivation = args: super.mkDerivation (args // {
      #enableLibraryProfiling = true;
    });
  };
}) // {
  stack2nix = pkgs.haskellPackages.callPackage ./pkgs/stack2nix.nix {};
}
