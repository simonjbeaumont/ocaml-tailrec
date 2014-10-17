default: tailrec

.PHONY: tailrec test_pass test_fail clean

tailrec:
	ocamlfind ocamlopt -package compiler-libs.common -o tailrec -linkpkg tailrec.ml

test_pass: tailrec
	ocamlc -c -bin-annot test/test_pass.ml
	./tailrec test/test_pass.cmt

test_fail: tailrec
	ocamlc -c -bin-annot test/test_fail.ml
	./tailrec test/test_fail.cmt

clean:
	rm -rf *.cmi *.cmx *.o tailrec
	rm -rf test/*.cmi test/*.cmo test/*.cmt
