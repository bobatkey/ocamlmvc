SRC := OCamlMVC.ml VCR.mli VCR.ml TodoList.ml TodoListComponent.mli TodoListComponent.ml Main.ml
CMO := $(patsubst %.ml,%.cmo,$(filter-out %.mli,$(SRC)))

all: todo.js

######################################################################
todo.byte: $(CMO)
	ocamlfind ocamlc -package js_of_ocaml -linkpkg -o $@ $^

######################################################################
clean:
	rm -f *.cmo *.cmi
	rm -f todo.byte todo.js
	rm -f *~

######################################################################
%.cmo: %.ml
	ocamlfind ocamlc -package js_of_ocaml \
                         -package js_of_ocaml.syntax \
                         -syntax camlp4o \
                         -c $<

%.cmi: %.mli
	ocamlfind ocamlc -package js_of_ocaml \
                         -package js_of_ocaml.syntax \
                         -syntax camlp4o \
                         -c $<

%.js: %.byte
	js_of_ocaml -opt 2 $<

######################################################################
.ocamldeps: $(SRC)
	@ocamlfind ocamldep -package js_of_ocaml.syntax \
                            -syntax camlp4o \
                            $(SRC) >.ocamldeps
-include .ocamldeps
