{
  description = "A nix flake for purescript-hydra-sdk.";

  nixConfig = {
    extra-experimental-features = [ "flakes" "nix-command" ];
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]hydra-sdk@\\[\\033[33m\\]$(git rev-parse --abbrev-ref HEAD) \\[\\e[0;32m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  };

  inputs = {
    nixpkgs.follows = "ctl/nixpkgs";
    hydra.url = "github:input-output-hk/hydra/0.19.0";
    hydra-fixtures = {
      url = "github:input-output-hk/hydra/85a210df73e15733c602a8c0c46aab2400d5323d";
      flake = false;
    };
    cardano-node.url = "github:input-output-hk/cardano-node/10.1.3";
    cardano-configurations = {
      url = "github:input-output-hk/cardano-configurations?rev=a913d87246dc2484562a00c86e5f9c74a20e82ce";
      flake = false;
    };
    ctl = {
      url = "github:Plutonomicon/cardano-transaction-lib/4bae6a202f3c77952d6067f94d8ae63cb74f3c0f";
      inputs = {
        cardano-node.follows = "cardano-node";
        cardano-configurations.follows = "cardano-configurations";
      };
    };
  };

  outputs = { self, nixpkgs, ctl, hydra, hydra-fixtures, ... }@inputs:
    let
      projectName = "purescript-hydra-sdk";
      supportedSystems = [ "x86_64-linux" ];

      pursBuildOptions = {
        strictComp = true;
        censorCodes = [
          "ImplicitImport"
          "ImplicitQualifiedImport"
          "ImplicitQualifiedImportReExport"
          "UserDefinedWarning"
          "MissingKindDeclaration"
          "MissingTypeDeclaration"
        ];
      };

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

      hydraFixturesFor = pkgs: pkgs.runCommand "hydra-fixtures" { buildInputs = [ pkgs.jq ]; }
        ''
          mkdir $out
          VALID_ADDR="KjgoiXJS2coTnqpCLHXFtd89Hv9ttjsE6yW4msyLXFNkykUpTsyBs85r2rDDia2uKrhdpGKCJnmFXwvPSWLe75564ixZWdTxRh7TnuaDLnHx"
          for fixture in ${hydra-fixtures}/hydra-node/golden/ServerOutput/*; do
            if [ -f "$fixture" ]; then
              echo "Fixing Hydra fixture: $fixture"
              jq --arg validAddr "$VALID_ADDR" \
                'walk(if type == "object" and has("address") and (.address | test("^addr_test1|^addr1") | not)
                then .address = $validAddr else . end)' \
                "$fixture" > tmp
              mv tmp "$out/$(basename "$fixture")"
            fi
          done
        '';

      minimalExampleFor = system: pkgs:
        pkgs.purescriptProject rec {
          inherit pkgs;
          projectName = "purescript-hydra-sdk-example-minimal";
          src = builtins.path {
            path = ./.;
            name = "${projectName}-src";
            filter = path: ftype:
              !(pkgs.lib.hasSuffix ".md" path) &&
              (builtins.baseNameOf path != "flake.nix") &&
              !(ftype == "directory" && builtins.baseNameOf path == "docker");
          };
          packageJson = ./example/minimal/package.json;
          packageLock = ./example/minimal/package-lock.json;
          spagoPackages = ./example/minimal/spago-packages.nix;
        };

      hydraSdkFor = system: pkgs: hydraFixtures:
        pkgs.purescriptProject rec {
          inherit pkgs projectName;
          src = builtins.path {
            path = ./.;
            name = "${projectName}-src";
            filter = path: ftype:
              !(pkgs.lib.hasSuffix ".md" path) &&
              (builtins.baseNameOf path != "flake.nix") &&
              !(ftype == "directory" && builtins.baseNameOf path == "example");
          };
          packageJson = ./package.json;
          packageLock = ./package-lock.json;
          shell = {
            withRuntime = true;
            shellHook = "export HYDRA_FIXTURES=${hydraFixtures}";
            packageLockOnly = true;
            packages = with pkgs; [
              fd
              jq
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
          hydraFixtures = hydraFixturesFor pkgs;
        in
        {
          default = (hydraSdkFor system pkgs hydraFixtures).devShell;
        }
      );

      packages = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          project = minimalExampleFor system pkgs;
          minimalExample = project.buildPursProject {
            inherit (pursBuildOptions) strictComp censorCodes;
          };
        in
        {
          hydra-sdk-example-minimal = pkgs.writeShellApplication {
            name = "hydra-sdk-example-minimal";
            text = ''
              TEMPD=$(mktemp -d)
              cd "$TEMPD"
              cp -r ${minimalExample}/* .
              ln -sfn ${project.nodeModules}/lib/node_modules node_modules
              node --enable-source-maps -e 'import("./output/HydraSdk.Example.Minimal.Main/index.js").then(m => m.main())' \
                -- hydra-sdk-example-minimal "$@"
            '';
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
          hydraFixtures = hydraFixturesFor pkgs;
          project = hydraSdkFor system pkgs hydraFixtures;
          builtProject = project.buildPursProject {
            inherit (pursBuildOptions) strictComp censorCodes;
          };
        in
        {
          hydra-sdk-unit-tests = project.runPursTest {
            inherit builtProject;
            name = "hydra-sdk-unit-tests";
            testMain = "Test.Main";
            env = { HYDRA_FIXTURES = "${hydraFixtures}"; };
          };

          hydra-sdk-formatting-check = pkgs.runCommand "hydra-sdk-formatting-check"
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
              prettier -c $(fd -ejs)
              nixpkgs-fmt --check $(fd -enix --exclude='spago*')
              touch $out
            '';
        }
      );
    };
}
