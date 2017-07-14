
# Usage:
# Type 'make bc' for byte code.
# Type 'make nc' for native code.
# Type 'make dc' for code with debugging information.
# For more detail, see http://mmottl.github.io/ocaml-makefile/

SOURCES=mySet.cmo syntax.ml parser.mly lexer.mll environment.mli environment.ml debug.ml eval.ml mySet.mli mySet.ml typing.ml main.ml
RESULT=miniml
YFLAGS=-v
OCAMLYACC=menhir

all: nc

include OCamlMakefile
