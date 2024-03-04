LIGO_COMPILER = docker run --rm -v "${PWD}":"${PWD}" -w "${PWD}" ligolang/ligo:1.4.0

compile:
	rm -r -f ./build 
	mkdir ./build
	${LIGO_COMPILER} compile contract contracts/kernel_governance.mligo -o build/kernel_governance.tz
	${LIGO_COMPILER} compile contract contracts/sequencer_governance.mligo -o build/sequencer_governance.tz
	mkdir ./build/test
	${LIGO_COMPILER} compile contract contracts/test/rollup_mock.mligo -o build/test/rollup_mock.tz
	${LIGO_COMPILER} compile contract contracts/test/internal_test_proxy.mligo -o build/test/internal_test_proxy.tz