stack = STACK_YAML="stack.yaml" stack
package = blacktip

build:
	$(stack) build

ghci:
	$(stack) ghci $(package):lib

ghcid:
	ghcid -c "$(stack) ghci $(package):lib"
