rec {
  # Functions which extend a haskellPackages with the packages local to this repository using haskellPackages.callPackage. Used later to make augmented
  # platform-specific package sets, but also useful for integrating Reflex Native into your Nix build environment.
  packages = haskellPackages: {
    kiwi-dsl = haskellPackages.callPackage ./kiwi/dsl {};
    kiwi-cpp = haskellPackages.callPackage ./kiwi/cpp {};
    hs-uikit = haskellPackages.callPackage ./hs-uikit {};
    reflex-native = haskellPackages.callCabal2nix "reflex-native" ./reflex-native {};
    reflex-native-draggy = haskellPackages.callPackage ./examples/draggy {};
    reflex-native-test = haskellPackages.callCabal2nix "reflex-native-test" ./reflex-native-test {};
    reflex-native-uikit = haskellPackages.callCabal2nix "reflex-native-uikit" ./reflex-native-uikit {};
  };

  # Version of reflex-platform we use for iteration on Reflex Native and compiling the examples
  reflex-platform-src = (import <nixpkgs> {}).fetchFromGitHub (builtins.fromJSON (builtins.readFile ./reflex-platform-version.json));

  # reflex-platform for iteration on Reflex Native and compiling the examples
  reflex-platform = import reflex-platform-src {};

  # Host nixpkgs from reflex-platform
  nixpkgs = reflex-platform.nixpkgs;

  # Alias to the iOS cross-building nixpkgs from reflex-platform. Useful when nix REPLing.
  iosAarch64 = reflex-platform.nixpkgsCross.ios.aarch64;

  # Functions which overlay a haskellPackages with the packages local to this repository and appropriate for the given platform using
  # haskellPackages.callPackage. Used later to make augmented platform-specific package sets, but also useful for integrating Reflex Native into your Nix
  # build environment.
  overrides = {
    common = self: super: {};

    host = nixpkgs.lib.composeExtensions overrides.common (self: super: packages self);

    android = nixpkgs.lib.composeExtensions overrides.common (self: super: packages self);

    ios = nixpkgs.lib.composeExtensions overrides.common (self: super: packages self);
  };

  # haskellPackages for the host extended with our local overrides.
  ghcHost = reflex-platform.ghc.override { overrides = overrides.host; };

  # haskellPackages for Android extended with our local overrides.
  ghcAndroidAarch64 = reflex-platform.ghcAndroidAarch64.override { overrides = overrides.android; };

  # haskellPackages for iOS extended with our local overrides.
  ghcIosAarch64 = reflex-platform.ghcIosAarch64.override { overrides = overrides.ios; };

  # Shell environments for the various platforms
  shells = let
    common = ["kiwi-dsl" "kiwi-cpp" "hs-uikit" "reflex-native" "reflex-native-draggy" "reflex-native-uikit"];
  in nixpkgs.lib.mapAttrs (k: drv: drv.overrideAttrs (_: { shellHook = "runHook preConfigureHooks; runHook setupHook"; })) {
    # Shell environment for working on the cross-platform bits only, notably the test framework.
    host = reflex-platform.workOnMulti' {
      env = ghcHost;
      packageNames = common ++ ["reflex-native-test"];
      tools = env: [ nixpkgs.cc ]; # without this, cc-wrapper isn't on PATH and so cabal can't detect that libstdc++ is around
    };

    # Shell environment for working on the Android side with Android related packages and common packages.
    android = (reflex-platform.workOnMulti' {
      env = ghcAndroidAarch64;
      packageNames = common ++ [];
      tools = env: [ nixpkgs.cc ]; # without this, cc-wrapper isn't on PATH and so cabal can't detect that libstdc++ is around
    });

    # Shell environment for working on the iOS side with the UIKit related packages, common packages, and any special environmental magics to get iOS cross
    # building working in a shell
    ios = (reflex-platform.workOnMulti' {
      env = ghcIosAarch64;
      packageNames = common ++ [];

      # special magics to get the preConfigureHook which adds the framework search paths for iOS frameworks
      # ideally this would not be necessary, and it isn't if haskellPackages generic-builder is doing the work, but since we're running cabal manually it's
      # needed
      tools = env: [
        nixpkgs.cc # without this, cc-wrapper isn't on PATH and so cabal can't detect that libstdc++ is around
        iosAarch64.buildPackages.darwin.xcode_8_2
      ];
    }).overrideAttrs (_: { shellHook = "runHook preConfigureHooks"; });
  };

  # Derivations for building each of the examples, grouped by the target platform
  examples = {
    # Derivations for building iOS app examples
    ios = {
      # Derivation for building the reflex-native-draggy example as a packaged iOS app.
      draggy = (reflex-platform.iosWithHaskellPackages ghcIosAarch64).buildApp {
        package = p: p.reflex-native-draggy;
        executableName = "reflex-native-draggy-uikit";
        bundleIdentifier = "org.reflexfrp.reflex-native-draggy";
        bundleName = "Reflex Native Draggy";
      };
    };
  };
}

