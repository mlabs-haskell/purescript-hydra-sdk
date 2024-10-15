.PHONY: build, format, repl

ps-sources := $(shell fd --no-ignore-parent -epurs)
nix-sources := $(shell fd --no-ignore-parent -enix --exclude='spago*')
purs-args := "--stash --censor-lib --censor-codes=ImplicitImport,ImplicitQualifiedImport,ImplicitQualifiedImportReExport,UserDefinedWarning"

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

repl: requires-nix-shell
	spago repl
