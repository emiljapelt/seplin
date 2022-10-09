ocamlc -c programRep.ml
ocamlyacc assemblyParser.mly
ocamllex assemblyLexer.mll
ocamlc -c assemblyParser.mli
ocamlc -c assemblyParser.ml
ocamlc -c assemblyLexer.ml
ocamlc -g programRep.cmo assemblyLexer.cmo assemblyParser.cmo assemblyWriter.ml -o inexac