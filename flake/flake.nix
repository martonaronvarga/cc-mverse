{
  description = "Multiverse analysis pipeline - R + Rust";

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
        pkgsMusl = import nixpkgs {
          inherit system;
          crossSystem = {
            config = "x86_64-unknown-linux-musl";
            isStatic = true;
          };
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
            processx
            qs2
            yaml
          ];
        };

        rustToolchainHost = fenix.packages.${system}.stable.withComponents [
          "rustc"
          "rust-src"
          "cargo"
          "clippy"
          "rustfmt"
        ];

        rustToolchainMusl = fenix.packages.${system}.combine [
          fenix.packages.${system}.stable.rustc
          fenix.packages.${system}.stable.cargo
          fenix.packages.${system}.targets."x86_64-unknown-linux-musl".stable.rust-std
        ];

        naersk-host = pkgs.callPackage naersk {
          rustc = rustToolchainHost;
          cargo = rustToolchainHost;
        };

        naersk-musl = pkgs.callPackage naersk {
          rustc = rustToolchainMusl;
          cargo = rustToolchainMusl;
        };
      in {
        packages.default = naersk-host.buildPackage {
          src = ../R/rust;
          nativeBuildInputs = [pkgs.pkg-config pkgs.gfortran];
          buildInputs = [pkgs.openssl pkgs.openblas];

          # Tell openblas-src to use the system library instead of building from source
          OPENBLAS_LIB_DIR = "${pkgs.openblas}/lib";
          OPENBLAS_INCLUDE_DIR = "${pkgs.openblas}/include";
          OPENBLAS_SYSTEM = "1";

          CARGO_FEATURE_SYSTEM_OPENBLAS = "1";
        };

        # Static musl binary for HPC deployment
        packages.process-static = naersk-musl.buildPackage {
          src = ../R/rust;
          nativeBuildInputs = with pkgs; [pkg-config gfortran];
          buildInputs = with pkgsMusl; [openssl.dev openblas stdenv.cc];

          CARGO_BUILD_TARGET = "x86_64-unknown-linux-musl";
          CARGO_BUILD_RUSTFLAGS = "-C target-feature=+crt-static";
          OPENBLAS_LIB_DIR = "${pkgsMusl.openblas}/lib";
          OPENBLAS_INCLUDE_DIR = "${pkgsMusl.openblas}/include";
          OPENBLAS_SYSTEM = "1";
          CARGO_FEATURE_SYSTEM_OPENBLAS = "1";

          OPENSSL_DIR = "${pkgsMusl.openssl.dev}";
          OPENSSL_LIB_DIR = "${pkgsMusl.openssl.out}/lib";
          OPENSSL_STATIC = "1";

          # Tell cargo/cc where the musl linker lives
          CARGO_TARGET_X86_64_UNKNOWN_LINUX_MUSL_LINKER = let
            cc = pkgsMusl.stdenv.cc;
          in "${cc}/bin/${cc.targetPrefix}cc";

          # Post-build verification
          doCheck = false; # Tests may not run under cross
          postInstall = ''
            echo "verifying static binary"
            file $out/bin/process
            # Ensure it's statically linked
            if ldd $out/bin/process 2>&1 | grep -q "not a dynamic"; then
              echo "OK: binary is statically linked"
            elif ! ldd $out/bin/process 2>/dev/null; then
              echo "OK: ldd reports not dynamic (static binary)"
            else
              echo "WARNING: binary may be dynamically linked"
              ldd $out/bin/process || true
            fi
          '';
        };

        devShells.default = pkgs.mkShell rec {
          name = "R";
          nativeBuildInputs = [pkgs.pkg-config];
          buildInputs = [
            R-dev
            pkgs.rlwrap
            rustToolchainHost
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
            echo "  nix build ./flake#process-static  — static musl binary for HPC"
            echo "  nix build                   — native debug/dev binary"
            echo "  ./rust_to_hpc.sh user@host  — deploy to HPC"
          '';
          LIBCLANG_PATH = "${pkgs.libclang.lib}/lib";
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (buildInputs ++ nativeBuildInputs);
          PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
        };
      }
    );
}
