debug:
	ocamlc -c utils.ml
	ocamlc -c runtime.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c eval.ml
	ocamlc -c compiler.ml
	ocamlc -o pri str.cma utils.cmo runtime.cmo parser.cmo lexer.cmo eval.cmo
	ocamlc -o prc str.cma utils.cmo runtime.cmo parser.cmo lexer.cmo compiler.cmo

release:
	ocamlopt -c utils.ml
	ocamlopt -c runtime.ml
	ocamlyacc parser.mly
	ocamlopt -c parser.mli
	ocamlopt -c parser.ml
	ocamllex lexer.mll
	ocamlopt -c lexer.ml
	ocamlopt -c eval.ml
	ocamlopt -c compiler.ml
	ocamlopt -o pri str.cmxa utils.ml runtime.ml parser.ml lexer.ml eval.ml
	ocamlopt -o prc str.cmxa utils.ml runtime.ml parser.ml lexer.ml compiler.ml

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