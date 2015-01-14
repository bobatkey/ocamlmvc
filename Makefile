TODO_SRC := OCamlMVC.mli OCamlMVC.ml VCR.mli VCR.ml TodoList.mli TodoList.ml TodoListComponent.mli TodoListComponent.ml Main.ml
TODO_CMO := $(patsubst %.ml,%.cmo,$(filter-out %.mli,$(TODO_SRC)))

EDITOR_SRC := OCamlMVC.mli OCamlMVC.ml VCR.mli VCR.ml TodoList.mli TodoList.ml TodoListComponent.mli TodoListComponent.ml Editor.ml EditorMain.ml
EDITOR_CMO := $(patsubst %.ml,%.cmo,$(filter-out %.mli,$(EDITOR_SRC)))

SRC := $(TODO_SRC) $(EDITOR_SRC)

all: todo.js editor.js

######################################################################
todo.byte: $(TODO_CMO)
	@echo Linking $@
	@ocamlfind ocamlc -package js_of_ocaml -linkpkg -o $@ $^

editor.byte: $(EDITOR_CMO)
	@echo Linking $@
	@ocamlfind ocamlc -package js_of_ocaml -linkpkg -o $@ $^

######################################################################
clean:
	rm -f *.cmo *.cmi
	rm -f todo.byte todo.js
	rm -f editor.byte editor.js
	rm -f *~

######################################################################
%.cmo: %.ml
	@echo Compiling $<
	@ocamlfind ocamlc -package js_of_ocaml \
                         -package js_of_ocaml.syntax \
                         -syntax camlp4o \
                         -c $<

%.cmi: %.mli
	@echo Compiling $<
	@ocamlfind ocamlc -package js_of_ocaml \
                         -package js_of_ocaml.syntax \
                         -syntax camlp4o \
                         -c $<

%.js: %.byte
	@echo Compiling $< to $@
	@js_of_ocaml --opt 2 $<

######################################################################
.ocamldeps: $(SRC)
	@ocamlfind ocamldep -package js_of_ocaml.syntax \
                            -syntax camlp4o \
                            $(SRC) >.ocamldeps
-include .ocamldeps
