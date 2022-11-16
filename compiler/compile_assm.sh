#!/bin/bash
ocamlc -c programRep.ml
ocamlc -c exceptions.ml
ocamlyacc assemblyParser.mly
ocamlc -c assemblyParser.mli
ocamlc -c assemblyParser.ml
ocamllex assemblyLexer.mll
ocamlc -c assemblyLexer.ml
ocamlc -c assemblyWriter.ml

ocamlc str.cma programRep.cmo exceptions.cmo assemblyParser.cmo assemblyLexer.cmo assemblyWriter.cmo absyn.cmo lexer.cmo parser.cmo toProgramRep.cmo compiler.ml -o inexc.exe