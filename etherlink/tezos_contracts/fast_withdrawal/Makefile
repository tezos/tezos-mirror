LIGO_COMPILER = docker run --rm -v "${PWD}":"${PWD}" -w "${PWD}" ligolang/ligo:1.9.2

compile:
	rm -r -f ./build
	mkdir ./build
	${LIGO_COMPILER} compile contract fast-withdrawal/fast-withdrawal.mligo -o build/fast-withdrawal.tz
