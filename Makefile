all:
	mkdir -p bin
	for f in *.hs; do \
		stack ghc $$f -- -Wall -Werror -O2 -no-keep-hi-files -no-keep-o-files -o bin/$$(basename $$f ".hs"); \
	done
