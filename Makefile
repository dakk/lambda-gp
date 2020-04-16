all:
	dune build src/lambda_gp.exe
clean:
	rm -rf _build
run:
	./_build/default/src/lambda_gp.exe