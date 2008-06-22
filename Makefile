OCAMLBUILD = ocamlbuild
NAME = onnt
TARGET = onnt

all: byte native test

native: 
	$(OCAMLBUILD) $(TARGET).cmxa

byte:
	$(OCAMLBUILD) $(TARGET).cma

doc:
	$(OCAMLBUILD) $(TARGET).docdir/index.html

test:
	$(OCAMLBUILD) test.native

clean:
	$(OCAMLBUILD) -clean

install:
	ocamlfind install $(NAME) *.mli _build/*.cmi _build/onnt.cma _build/onnt.cmxa META