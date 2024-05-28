init: lexer.mll parser.mly
	ocamlyacc parser.mly
	ocamllex lexer.mll
	rm parser.mli
	ocamlc -c parser.ml
	ocamlc -c lexer.ml
	ocamlc -c intrprt.ml
	ocamlc -c main.ml
	ocamlc -o main parser.cmo lexer.cmo intrprt.cmo main.cmo
	clear


prolog:
	./main $(file)

clean:
	rm -f *.cmo *.cmi parser.ml lexer.ml  main test intrprt

test: test.ml
	ocamlc -c test.ml
	ocamlc -o test parser.cmo lexer.cmo intrprt.cmo test.cmo
	./test