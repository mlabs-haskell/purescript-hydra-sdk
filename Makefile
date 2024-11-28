.PHONY: build, format, repl, docs, build-example, run-example, docker-cleanup

ps-sources := $(shell fd --no-ignore-parent -epurs)
nix-sources := $(shell fd --no-ignore-parent -enix --exclude='spago*')
purs-args := "--stash --censor-lib --censor-codes=ImplicitImport,ImplicitQualifiedImport,ImplicitQualifiedImportReExport,UserDefinedWarning"
example-docker := example/minimal/docker/cluster/docker-compose.yaml

system := $(shell uname -s)
ifeq (${system},Linux)
    open-in-browser := xdg-open
else
    open-in-browser := open
endif

requires-nix-shell:
	@[ "$(IN_NIX_SHELL)" ] || \
		( echo "The '$(MAKECMDGOALS)' target must be run from inside a nix shell, run 'nix develop' first." \
				&& false \
		)

build: requires-nix-shell
	spago build --purs-args ${purs-args}

format: requires-nix-shell
	@purs-tidy format-in-place ${ps-sources}
	@nixpkgs-fmt ${nix-sources}
	doctoc README.md --github --notitle

repl: requires-nix-shell
	spago repl

docs:
	nix build .#docs
	${open-in-browser} result/generated-docs/html/index.html

build-example:
	cd example/minimal && \
		spago build --purs-args ${purs-args}

run-example: docker-cleanup
	docker compose -f ${example-docker} up --build --no-attach cardano-node

docker-cleanup:
	docker compose -f ${example-docker} rm --force --stop
	docker volume rm -f cluster_hydra-persist-a cluster_hydra-persist-b
