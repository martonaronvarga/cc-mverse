{
  description = "R with helix flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    naersk.inputs.nixpkgs.follows = "nixpkgs";
    fenix.url = "github:nix-community/fenix";
    fenix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    nixpkgs,
    utils,
    naersk,
    fenix,
    self,
    ...
  }:
    utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
        };
        R-dev = pkgs.rWrapper.override {
          packages = with pkgs.rPackages; [
            languageserver
            tidyverse
            patchwork
            lintr
            styler
            testthat
            roxygen2
            targets
            tarchetypes
            visNetwork
            crew
            crew_cluster
            afex
            arrow
            broom
            broom_mixed
            logger
            autometric
            rmarkdown
            kableExtra
            glue
            future
            future_batchtools
            #for testing:
            data_table
            lme4
            lmerTest
            furrr
            xgboost
            matrixStats
            progressr
            qs2
          ];
        };

        rustToolchain = fenix.packages.${system}.stable.withComponents [
          "rustc"
          "rust-src"
          "cargo"
          "clippy"
          "rustfmt"
        ];

        naersk-lib = pkgs.callPackage naersk {
          rustc = rustToolchain;
          cargo = rustToolchain;
        };
      in {
        packages.default = naersk-lib.buildPackage {src = ./rust;};
        devShells.default = pkgs.mkShell rec {
          name = "R";
          nativeBuildInputs = [pkgs.pkg-config];
          buildInputs = [
            R-dev
            pkgs.rlwrap
            rustToolchain
            pkgs.rust-analyzer
            pkgs.openssl
            pkgs.cargo
            pkgs.clang
            pkgs.libclang
            pkgs.libgcc.lib
            pkgs.pandoc
          ];
          shellHook = ''
            echo "dev shell with R & Rust available"

          '';
          LIBCLANG_PATH = "${pkgs.libclang.lib}/lib";
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (buildInputs ++ nativeBuildInputs);
          PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
        };

        packages.pipeline-runner = pkgs.stdenv.writeShellApplication {
          pname = "pipeline-runner";
          runtimeInputs = [R-dev];
          text = ''
            set -euo pipefail
            REPO="$PWD"
            cd "$REPO/R"
            exec Rscript run.R "$@"
          '';
        };

        apps.pipeline = {
          type = "app";
          program = "${self.packages.${system}.pipeline-runner}/bin/run-pipeline";
        };
      }
    );
}
