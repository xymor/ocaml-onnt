OCAMLBUILD = ocamlbuild
TARGET = onnt

all: byte native doc

native: 
	$(OCAMLBUILD) $(TARGET).cmxa

byte:
	$(OCAMLBUILD) $(TARGET).cma

doc:
	$(OCAMLBUILD) $(TARGET).docdir/index.html

clean:
	$(OCAMLBUILD) -clean