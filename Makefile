.PHONY: all clean

all: static.ml
	@rm -f tapkaz.bin
	mir tapkaz.bin
	@ln -s _build/tapkaz.bin .

static.ml: files/*
	mlcrunch files/ > $@

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml tapkaz.bin static.ml