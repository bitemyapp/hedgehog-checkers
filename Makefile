stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

build:
	$(stack) build

test:
	$(stack) test

upload:
	$(stack) upload hedgehog-checkers
	$(stack) upload hedgehog-checkers-lens

.PHONY : build test

