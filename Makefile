.PHONY: build, test, format, repl, docs, build-example, run-example, docker-cleanup, gen-keys

ps-sources := $(shell fd --no-ignore-parent -epurs)
nix-sources := $(shell fd --no-ignore-parent -enix --exclude='spago*')
purs-args := "--stash --censor-lib --censor-codes=ImplicitImport,ImplicitQualifiedImport,ImplicitQualifiedImportReExport,UserDefinedWarning"
example-docker := example/minimal/docker/cluster/docker-compose.yaml
example-keys := example/minimal/docker/cluster/keys/

requires-nix-shell:
	@[ "$(IN_NIX_SHELL)" ] || \
		( echo "The '$(MAKECMDGOALS)' target must be run from inside a nix shell, run 'nix develop' first." \
				&& false \
		)

build: requires-nix-shell
	spago build --purs-args ${purs-args}

test: requires-nix-shell
	spago run --main Test.Main

format: requires-nix-shell
	@purs-tidy format-in-place ${ps-sources}
	@nixpkgs-fmt ${nix-sources}
	doctoc README.md --github --notitle

repl: requires-nix-shell
	spago repl

docs: requires-nix-shell
	mv package.json package.json.old
	jq 'del(.type)' package.json.old > package.json
	spago docs --open
	mv -f package.json.old package.json

build-example: requires-nix-shell
	cd example/minimal && \
		spago build --purs-args ${purs-args}

run-example: docker-cleanup
	docker compose -f ${example-docker} up --build --no-attach cardano-node

docker-cleanup:
	docker compose -f ${example-docker} rm --force --stop
	docker volume rm -f cluster_hydra-persist-a cluster_hydra-persist-b

gen-keys: requires-nix-shell
	@hydra-node gen-hydra-key --output-file ${example-keys}/hydra-a
	@hydra-node gen-hydra-key --output-file ${example-keys}/hydra-b
	@cardano-cli address key-gen \
		--signing-key-file ${example-keys}/cardano-a.sk \
		--verification-key-file ${example-keys}/cardano-a.vk
	@cardano-cli address build \
		--payment-verification-key-file ${example-keys}/cardano-a.vk \
		--testnet-magic 1
	@echo
	@cardano-cli address key-gen \
		--signing-key-file ${example-keys}/cardano-b.sk \
		--verification-key-file ${example-keys}/cardano-b.vk
	@cardano-cli address build \
		--payment-verification-key-file ${example-keys}/cardano-b.vk \
		--testnet-magic 1
	@echo
