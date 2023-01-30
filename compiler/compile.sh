#!/bin/bash
ocamlc -c programRep.ml &&
ocamlc -c exceptions.ml &&
ocamlyacc assemblyParser.mly &&
ocamlc -c assemblyParser.mli &&
ocamlc -c assemblyParser.ml &&
ocamllex assemblyLexer.mll &&
ocamlc -c assemblyLexer.ml &&
ocamlc -c assemblyWriter.ml &&

ocamlc -c absyn.ml &&
ocamlyacc parser.mly &&
ocamlc -c parser.mli &&
ocamlc -c parser.ml &&
ocamllex lexer.mll &&
ocamlc -c lexer.ml &&
ocamlc -c helpers.ml &&
ocamlc -c typing.ml &&
ocamlc -c toProgramRep.ml &&

ocamlc str.cma programRep.cmo exceptions.cmo assemblyParser.cmo assemblyLexer.cmo assemblyWriter.cmo absyn.cmo lexer.cmo parser.cmo helpers.cmo typing.cmo toProgramRep.cmo compiler.ml -o inexc.exe &&
mkdir -p ./bin &&
mv ./*.cmo ./bin &&
mv ./*.cmi ./bin