# You can set these variables from the command line.
SPHINXOPTS    = -aE -n
SPHINXBUILD   = poetry run sphinx-build
SPHINXPROJ    = Tezos
SOURCEDIR     = .
BUILDDIR      = _build

DOCGENDIR     = doc_gen
DOCERRORDIR   = $(DOCGENDIR)/errors
DOCRPCDIR     = $(DOCGENDIR)/rpcs

ALPHA_LONG    = ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK
GRANADA_LONG = PtGRANADsDU8R9daYKAgWnQYAJ64omN1o3KMGVCykShA97vQbvV
FLORENCE_LONG = PsFLorenaUUuikDWvMDr6fGBRG8kt3e3D3fHoXK1j1BFRxeSH4i
EDO_LONG      = PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA

ALPHA_SHORT    = alpha
GRANADA_SHORT = 010-PtGRANAD
FLORENCE_SHORT = 009-PsFLoren
EDO_SHORT      = 008-PtEdo2Zk

SCRIPTSDIR		= scripts
CHECKXREFS		= $(SCRIPTSDIR)/check_proto_xrefs.py

all: manuals odoc html

main:
	@${MAKE} -C .. all

manuals: main
    # alpha protocol
	@../tezos-client -protocol $(ALPHA_LONG) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > alpha/tezos-client-alpha.html
	@../tezos-baker-$(ALPHA_SHORT) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > alpha/tezos-baker-alpha.html
	@../tezos-endorser-$(ALPHA_SHORT) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > alpha/tezos-endorser-alpha.html
	@../tezos-accuser-$(ALPHA_SHORT) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > alpha/tezos-accuser-alpha.html
    # 010 (Granada) protocol
	@../tezos-client -protocol $(GRANADA_LONG) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 010/tezos-client-010.html
	@../tezos-baker-$(GRANADA_SHORT) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 010/tezos-baker-010.html
	@../tezos-endorser-$(GRANADA_SHORT) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 010/tezos-endorser-010.html
	@../tezos-accuser-$(GRANADA_SHORT) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 010/tezos-accuser-010.html
    # 009 (Florence) protocol
	@../tezos-client -protocol $(FLORENCE_LONG) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 009/tezos-client-009.html
	@../tezos-baker-$(FLORENCE_SHORT) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 009/tezos-baker-009.html
	@../tezos-endorser-$(FLORENCE_SHORT) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 009/tezos-endorser-009.html
	@../tezos-accuser-$(FLORENCE_SHORT) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 009/tezos-accuser-009.html
    # 008 (Edo) protocol
	@../tezos-client -protocol $(EDO_LONG) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 008/tezos-client-008.html
	@../tezos-baker-$(EDO_SHORT) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 008/tezos-baker-008.html
	@../tezos-endorser-$(EDO_SHORT) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 008/tezos-endorser-008.html
	@../tezos-accuser-$(EDO_SHORT) man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 008/tezos-accuser-008.html
    # generic
	@../tezos-admin-client man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > api/tezos-admin-client.html
	@../tezos-signer man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > api/tezos-signer.html
	@../tezos-codec man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > api/tezos-codec.html
	@../tezos-snoop man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > api/tezos-snoop.html
	../tezos-node run --help | sed "s#\(.\)$$(echo "\010")\1#\1#g; s#_$$(echo "\010")##g" > api/tezos-node-run.txt

.PHONY: odoc
odoc: main
	@cd $$(pwd)/.. ; dune build @doc
	@rm -rf $$(pwd)/_build/api/odoc
	@mkdir -p $$(pwd)/_build/api
	@cp -r $$(pwd)/../_build/default/_doc $$(pwd)/_build/api/odoc
	@echo '.toc {position: static}' >> $$(pwd)/_build/api/odoc/_html/odoc.css
	@echo '.content { margin-left: 4ex }' >> $$(pwd)/_build/api/odoc/_html/odoc.css
	@echo '@media (min-width: 745px) {.content {margin-left: 4ex}}' >> $$(pwd)/_build/api/odoc/_html/odoc.css
	@sed -e 's/@media only screen and (max-width: 95ex) {/@media only screen and (max-width: 744px) {/' $$(pwd)/_build/api/odoc/_html/odoc.css > $$(pwd)/_build/api/odoc/_html/odoc.css2
	@mv $$(pwd)/_build/api/odoc/_html/odoc.css2  $$(pwd)/_build/api/odoc/_html/odoc.css

linkcheck:
	$(SPHINXBUILD) -b linkcheck "$(SOURCEDIR)" "$(BUILDDIR)"

.PHONY: redirectcheck
redirectcheck:
	@cd $$(pwd)/.. ; ./src/tooling/lint.sh --check-redirects

xrefscheck:
	$(CHECKXREFS) 009
	$(CHECKXREFS) 010
	$(CHECKXREFS) -l alpha

scriptsindoccheck:
	introduction/test_howtoget.sh install-bin-bionic
	introduction/test_howtoget.sh install-bin-focal
	introduction/test_howtoget.sh install-bin-fedora31
	introduction/test_howtoget.sh install-bin-fedora32
	introduction/test_howtoget.sh install-opam-scratch
	introduction/test_howtoget.sh install-opam-bionic
	introduction/test_howtoget.sh install-opam-focal
	introduction/test_howtoget.sh compile-sources-buster
	#introduction/test_howtoget.sh use-docker # returns 1 (and eats alot of disk)

sanitycheck:
	@if grep -Rq alpha 0*; then					\
		echo "'alpha' occurs in the docs for released protocols:";	\
                grep -R alpha 0*;                                               \
		exit 1;								\
	fi

api/errors.rst: $(DOCERRORDIR)/error_doc.ml
	@cd .. && dune build docs/$(DOCERRORDIR)/error_doc.exe
	../_build/default/docs/$(DOCERRORDIR)/error_doc.exe > api/errors.rst

$(DOCGENDIR)/rpc_doc.exe:
	@cd .. && dune build docs/$(DOCGENDIR)/rpc_doc.exe

rpc: $(DOCGENDIR)/rpc_doc.exe
	@dune exec $(DOCGENDIR)/rpc_doc.exe "" > 009/rpc.rst
	@dune exec $(DOCGENDIR)/rpc_doc.exe 010 > 010/rpc.rst
	@dune exec $(DOCGENDIR)/rpc_doc.exe alpha > alpha/rpc.rst
	@dune exec $(DOCGENDIR)/rpc_doc.exe shell > shell/rpc.rst

$(DOCGENDIR)/p2p_doc.exe:
	@cd .. && dune build docs/$(DOCGENDIR)/p2p_doc.exe

p2p: $(DOCGENDIR)/p2p_doc.exe shell/p2p_usage.rst.inc
	@dune exec $(DOCGENDIR)/p2p_doc.exe < shell/p2p_usage.rst.inc > shell/p2p_api.rst

.PHONY: help Makefile

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
html: Makefile api/errors.rst rpc p2p
	@$(SPHINXBUILD) -b html "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)

clean:
	@-rm -Rf "$(BUILDDIR)"
	@-rm -Rf api/errors.rst 009/rpc.rst 010/rpc.rst alpha/rpc.rst shell/rpc.rst shell/p2p_api.rst CHANGES.rst
	@-rm -Rf api/tezos-*.html api/tezos-*.txt 009/tezos-*.html 010/tezos-*.html alpha/tezos-*.html
