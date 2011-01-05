debug:
	ocamlc -c type.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c utils.ml
	ocamlc -c eval.ml
	ocamlc -o primer.exe type.cmo lexer.cmo parser.cmo utils.cmo eval.cmo

release:
	ocamlopt -c type.ml
	ocamlopt -c type.ml
	ocamlyacc parser.mly
	ocamlopt -c parser.mli
	ocamlopt -c parser.ml
	ocamllex lexer.mll
	ocamlopt -c lexer.ml
	ocamlopt -c utils.ml
	ocamlopt -c eval.ml
	ocamlopt -o primer.exe type.ml lexer.ml parser.ml utils.ml eval.ml

clean:
	rm primer lexer.ml parser.ml parser.mli *.cmo *.cmi *.exe *.o *.cmx