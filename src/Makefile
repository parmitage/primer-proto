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
	ocamlc -c compiler.ml
	ocamlc -o pri str.cma utils.cmo type.cmo symtbl.cmo environment.cmo \
		parser.cmo lexer.cmo eval.cmo
	ocamlc -o prc str.cma utils.cmo type.cmo symtbl.cmo environment.cmo \
		parser.cmo lexer.cmo compiler.cmo

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
	ocamlopt -c compiler.ml
	ocamlopt -o pri str.cmxa utils.ml type.ml symtbl.ml environment.ml \
		parser.ml lexer.ml eval.ml
	ocamlopt -o prc str.cmxa utils.ml type.ml symtbl.ml environment.ml \
		parser.ml lexer.ml compiler.ml

clean:
	rm pri prc lexer.ml parser.ml parser.mli *.cmo *.cmi *.o *.cmx

docs:
	pandoc readme.md -o index.html -c doc.css

examples:
	./prc ../examples/dictionary.pri ../examples/dictionary.html
	./prc ../examples/euler.pri ../examples/euler.html
	./prc ../examples/factorial.pri ../examples/factorial.html
	./prc ../examples/fizzbuzz.pri ../examples/fizzbuzz.html
	./prc ../examples/knapsack.pri ../examples/knapsack.html
	./prc ../examples/object.pri ../examples/object.html
	./prc ../examples/rover.pri ../examples/rover.html
	./prc ../examples/search.pri ../examples/search.html