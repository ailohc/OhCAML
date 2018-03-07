all: run

run: lang.cmo lexer.cmo parser.cmo sym_eval.cmo solve.cmo main.cmo
	ocamlc -o run lexer.cmo parser.cmo lang.cmo sym_eval.cmo solve.cmo main.cmo

lang.cmo: lang.ml
	ocamlc -c lang.ml

sym_eval.cmo: sym_eval.ml
	ocamlc -c sym_eval.ml

solve.cmo: solve.ml
	ocamlc -c solve.ml

parser.ml: parser.mly sym_eval.cmo
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

main.cmo : sym_eval.cmo main.ml
	ocamlc -c main.ml

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

lexer.ml: lexer.mll parser.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo
