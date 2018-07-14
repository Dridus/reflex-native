rec {
  # Version of reflex-platform we use for iteration on Reflex Native and compiling the examples
  reflex-platform-src = (import <nixpkgs> {}).fetchFromGitHub (builtins.fromJSON (builtins.readFile ./reflex-platform-version.json));

  # reflex-platform for iteration on Reflex Native and compiling the examples
  reflex-platform = import reflex-platform-src {};

  # Host nixpkgs from reflex-platform
  nixpkgs = reflex-platform.nixpkgs;

  # Alias to the iOS cross-building nixpkgs from reflex-platform. Useful when nix REPLing.
  iosArm64 = reflex-platform.nixpkgsCross.ios.arm64.pkgs;

  # Functions which overlay a haskellPackages with the packages local to this repository and appropriate for the given platform using
  # haskellPackages.callPackage. Used later to make augmented platform-specific package sets, but also useful for integrating Reflex Native into your Nix
  # build environment.
  overrides = let
    inherit (nixpkgs) fetchFromGitHub;
    inherit (nixpkgs.lib) composeExtensions;
    inherit (nixpkgs.haskell.lib) dontCheck enableCabalFlag;
  in rec {
    common = self: super: {
      rank2classes = dontCheck (self.callCabal2nix "rank2classes" (fetchFromGitHub {
        owner = "blamario";
        repo = "grampa";
        rev = "f35d8882ee6a60e91a86db339bdac94710d8bc6b";
        sha256 = "1ssv0lrbbj694rficrka56l628ha9l61wrnxqxy6yn9dawk6h6n8";
      } + /rank2classes) {});

      reflex = enableCabalFlag (self.callPackage (fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex";
        rev = "9fcbf0792702f48185736cd4bebc2973f299e848";
        sha256 = "1p5b7gp1vwhq1slhfgbdlrgk5xll431rkzg3bzq15j8k9qy4b2bc";
      }) { useTemplateHaskell = false; }) "fast-weak";

      kiwi-dsl = self.callCabal2nix "kiwi-dsl" ./kiwi/dsl {};
      reflex-native = self.callCabal2nix "reflex-native" ./reflex-native {};
      reflex-native-draggy = self.callPackage ./examples/draggy {};
      reflex-native-test = self.callCabal2nix "reflex-native-test" ./reflex-native-test {};
    };

    host = composeExtensions common (self: super: {
      kiwi-cpp = self.callCabal2nix "kiwi-cpp" ./kiwi/binding/cpp {};
    });

    android = composeExtensions common (self: super: {
      kiwi-cpp = self.callCabal2nix "kiwi-cpp" ./kiwi/binding/cpp {};
    });

    ios = composeExtensions common (self: super: {
      kiwi-cpp = self.callCabal2nix "kiwi-cpp" ./kiwi/binding/cpp {};
      hs-uikit = self.callPackage ./hs-uikit {};
      reflex-native-uikit = self.callCabal2nix "reflex-native-uikit" ./reflex-native-uikit {};
    });
  };

  # haskellPackages for the host extended with our local overrides.
  ghcHost = reflex-platform.ghc8_2_1.override { overrides = overrides.host; };

  # haskellPackages for Android extended with our local overrides.
  ghcAndroidArm64 = reflex-platform.ghcAndroidArm64.override { overrides = overrides.android; };

  # haskellPackages for iOS extended with our local overrides.
  ghcIosArm64 = reflex-platform.ghcIosArm64.override { overrides = overrides.ios; };

  # Shell environments for the various platforms
  shells = let
    common = ["kiwi-dsl" "reflex-native" "reflex-native-draggy"];
  in nixpkgs.lib.mapAttrs (k: drv: drv.overrideAttrs (_: { shellHook = "runHook preConfigureHooks; runHook setupHook"; })) {
    # Shell environment for working on the cross-platform bits only, notably the test framework.
    host = reflex-platform.workOnMulti' {
      env = ghcHost;
      packageNames = common ++ ["kiwi-cpp" "reflex-native-test"];
      tools = env: [ nixpkgs.cc ]; # without this, cc-wrapper isn't on PATH and so cabal can't detect that libstdc++ is around
    };

    # Shell environment for working on the Android side with Android related packages and common packages.
    android = reflex-platform.workOnMulti' {
      env = ghcAndroidArm64;
      packageNames = common ++ ["kiwi-cpp"];
      tools = env: [ nixpkgs.cc ]; # without this, cc-wrapper isn't on PATH and so cabal can't detect that libstdc++ is around
    };

    # Shell environment for working on the iOS side with the UIKit related packages, common packages, and any special environmental magics to get iOS cross
    # building working in a shell
    ios = reflex-platform.workOnMulti' {
      env = ghcIosArm64;
      packageNames = common ++ ["kiwi-cpp" "hs-uikit" "reflex-native-uikit"];
      tools = env: [
        nixpkgs.cc # without this, cc-wrapper isn't on PATH and so cabal can't detect that libstdc++ is around
        iosArm64.buildPackages.osx_sdk # without this, the preConfigure hook isn't augmented to include the iOS SDK framework search paths
      ];
    };
  };

  # Derivations for building each of the examples, grouped by the target platform
  examples = {
    # Derivations for building iOS app examples
    ios = {
      # Derivation for building the reflex-native-draggy example as a packaged iOS app.
      draggy = (reflex-platform.iosWithHaskellPackages ghcIosArm64).buildApp {
        package = p: p.reflex-native-draggy;
        executableName = "reflex-native-draggy-uikit";
        bundleIdentifier = "org.reflexfrp.reflex-native-draggy";
        bundleName = "Reflex Native Draggy";
      };
    };
  };
}

