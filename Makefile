debug:
	ocamlc -c utils.ml
	ocamlc -c type.ml
	ocamlc -c symtbl.ml
	ocamlc -c environment.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c eval.ml
	ocamlc -o primer utils.cmo type.cmo symtbl.cmo environment.cmo parser.cmo lexer.cmo eval.cmo

release:
	ocamlopt -c utils.ml
	ocamlopt -c type.ml
	ocamlopt -c symtbl.ml
	ocamlopt -c environment.ml
	ocamlyacc parser.mly
	ocamlopt -c parser.mli
	ocamlopt -c parser.ml
	ocamllex lexer.mll
	ocamlopt -c lexer.ml
	ocamlopt -c eval.ml
	ocamlopt -o primer.exe utils.ml type.ml symtbl.ml environment.ml parser.ml lexer.ml eval.ml

clean:
	rm primer lexer.ml parser.ml parser.mli *.cmo *.cmi *.exe *.o *.cmx