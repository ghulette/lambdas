OCAMLMAKEFILE = ./OCamlMakefile

RESULT  = untyped
SOURCES = env.ml \
          term.ml \
					parser.mly \
					lexer.mll \
					interp.ml \
					main.ml

include $(OCAMLMAKEFILE)
