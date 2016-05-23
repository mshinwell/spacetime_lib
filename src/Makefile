ifdef WITH_BIN_ANNOT
	OCAMLFLAGS += -bin-annot
endif

ifdef WITH_DEBUG
	OCAMLFLAGS += -g
	OCAMLLDFLAGS += -g
endif

RESULT=spacetime_lib

SOURCES=spacetime_lib.mli spacetime_lib.ml

PACKS=raw_spacetime_lib

all: native-code-library

-include OCamlMakefile
