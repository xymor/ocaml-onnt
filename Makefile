OCAMLBUILD = ocamlbuild
TARGET = onnt

all: byte native doc top

native: 
	$(OCAMLBUILD) $(TARGET).cmxa

byte:
	$(OCAMLBUILD) $(TARGET).cma

top:
	$(OCAMLBUILD) $(TARGET).top

doc:
	$(OCAMLBUILD) $(TARGET).docdir/index.html

clean:
	$(OCAMLBUILD) -clean