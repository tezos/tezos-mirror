all: tezos-basic.jsonnet

%.jsonnet:
	jsonnet -J vendors/grafonnet-lib/grafonnet src/$@ > output/$*.generated

clean:
	rm output/*.generated
