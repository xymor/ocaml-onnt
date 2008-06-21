OCAMLBUILD = ocamlbuild
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