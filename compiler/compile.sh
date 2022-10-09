#!/bin/bash
ocamlc -c programRep.ml
ocamlyacc assemblyParser.mly
ocamlc -c assemblyParser.mli
ocamlc -c assemblyParser.ml
ocamllex assemblyLexer.mll
ocamlc -c assemblyLexer.ml
ocamlc -c assemblyWriter.ml

ocamlc -c absyn.ml
ocamlyacc parser.mly
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamllex lexer.mll
ocamlc -c lexer.ml
ocamlc -c toProgramRep.ml

ocamlc str.cma programRep.cmo assemblyParser.cmo assemblyLexer.cmo assemblyWriter.cmo absyn.cmo lexer.cmo parser.cmo toProgramRep.cmo compiler.ml -o inexc.exe