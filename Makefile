all:
	dune build 
clean:
	rm -rf _build
run:
	./_build/default/examples/$(BIN).exe