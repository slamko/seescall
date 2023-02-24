all: main

main: bin/main.cmx bin/main.ml
	ocamlopt unix.cmxa str.cmxa bin/main.cmx -o $@

bin/main.cmx: bin/main.ml
	ocamlopt -c $^ -o $@
