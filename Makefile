.PHONY: all clean install build
all: build doc

NAME=fat-filesystem
J=4

include config.mk

export OCAMLRUNPARAM=b

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	@./setup.bin -configure --enable-tests

build: setup.data setup.bin
	@./setup.bin -build -j $(J)

doc: setup.data setup.bin
	@./setup.bin -doc -j $(J)

install: setup.bin
	@./setup.bin -install
	cp ./_build/fat/main.native $(BINDIR)/fat

uninstall:
	@ocamlfind remove $(NAME) || true
	rm -f $(BINDIR)/fat

test: setup.bin build
	@./setup.bin -test

reinstall: setup.bin
	@ocamlfind remove $(NAME) || true
	@./setup.bin -reinstall

clean:
	@ocamlbuild -clean
	@rm -f setup.data setup.log setup.bin

# If we haven't run configure already, run it now with the
# defaults.
config.mk:
	./configure
