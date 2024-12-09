{
  description = "A nix flake for purescript-hydra-sdk.";

  nixConfig = {
    extra-experimental-features = [ "flakes" "nix-command" ];
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]purs-hydra-sdk@\\[\\033[33m\\]$(git rev-parse --abbrev-ref HEAD) \\[\\e[0;32m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  };

  inputs = {
    nixpkgs.follows = "ctl/nixpkgs";
    cardano-node.url = "github:input-output-hk/cardano-node/9.2.0";
    ctl.url = "github:Plutonomicon/cardano-transaction-lib/4bae6a202f3c77952d6067f94d8ae63cb74f3c0f";
    ctl.inputs.cardano-node.follows = "cardano-node";
    hydra.url = "github:input-output-hk/hydra/0.19.0";
  };

  outputs = { self, nixpkgs, ctl, hydra, hydra-auction-onchain, ... }@inputs:
    let
      projectName = "purescript-hydra-sdk";
      supportedSystems = [ "x86_64-linux" ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          ctl.overlays.purescript
          ctl.overlays.runtime
          ctl.overlays.spago
          (_: _: {
            arion = (import ctl.inputs.nixpkgs-arion { inherit system; }).arion;
          })
        ];
      };

      psProjectFor = system: pkgs:
        pkgs.purescriptProject rec {
          inherit pkgs projectName;
          src = builtins.path {
            path = ./.;
            name = "${projectName}-src";
            filter = path: ftype: !(pkgs.lib.hasSuffix ".md" path) && (builtins.baseNameOf path != "flake.nix");
          };
          packageJson = ./package.json;
          packageLock = ./package-lock.json;
          shell = {
            withRuntime = true;
            packageLockOnly = true;
            packages = with pkgs; [
              fd
              hydra.packages.${system}.hydra-node
              nixpkgs-fmt
              nodePackages.prettier
              nodePackages.purs-tidy
              inputs.cardano-node.packages.${system}."preview/node"
              inputs.cardano-node.packages.${system}."preprod/node"
            ];
          };
        };
    in
    {
      devShells = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        {
          default = (psProjectFor system pkgs).devShell;
        }
      );

      packages = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          project = psProjectFor system pkgs;
        in
        {
          docs = project.buildPursDocs {
            packageName = projectName;
          };
        }
      );

      apps = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          runtimeConfig = final: with final; {
            network.name = "preprod";
          };
        in
        {
          ctl-runtime = pkgs.launchCtlRuntime runtimeConfig;
        }
      );

      checks = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          project = psProjectFor system pkgs;
          builtProject = project.buildPursProject {
            strictComp = true;
            censorCodes = [
              "ImplicitImport"
              "ImplicitQualifiedImport"
              "ImplicitQualifiedImportReExport"
              "UserDefinedWarning"
            ];
          };
        in
        {
          formatting-check = pkgs.runCommand "formatting-check"
            {
              nativeBuildInputs = with pkgs; [
                fd
                easy-ps.purs-tidy
                nixpkgs-fmt
                nodePackages.prettier
              ];
            }
            ''
              cd ${self}
              purs-tidy check $(fd -epurs)
              nixpkgs-fmt --check $(fd -enix --exclude='spago*')
              prettier -c $(fd -ejs)
              touch $out
            '';
        }
      );
    };
}
