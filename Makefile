all:
	dune build @install  --profile release
test:
	dune build @runtest
clean:
	rm -rf _build


.PHONY : pin
pin: 
	opam pin add caravand . -n --working-dir && opam remove caravand && opam install caravand --working-dir
