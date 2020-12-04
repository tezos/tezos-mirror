# You can set these variables from the command line.
SPHINXOPTS    = -aE -n
SPHINXBUILD   = poetry run sphinx-build
SPHINXPROJ    = Tezos
SOURCEDIR     = .
BUILDDIR      = _build

DOCGENDIR     = doc_gen
DOCERRORDIR   = $(DOCGENDIR)/errors
DOCRPCDIR     = $(DOCGENDIR)/rpcs

all: manuals odoc html

main:
	@${MAKE} -C .. all

manuals: main
    # alpha protocol
	@../tezos-client -protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 008/tezos-client-alpha.html
	@../tezos-baker-alpha man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 008/tezos-baker-alpha.html
	@../tezos-endorser-alpha man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 008/tezos-endorser-alpha.html
	@../tezos-accuser-alpha man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 008/tezos-accuser-alpha.html
    # 007 delphi
	@../tezos-client -protocol PsDELPH1Kxsxt8f9eWbxQeRxkjfbxoqM52jvs5Y5fBxWWh4ifpo man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 007/tezos-client-007.html
	@../tezos-baker-alpha man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 007/tezos-baker-007.html
	@../tezos-endorser-alpha man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 007/tezos-endorser-007.html
	@../tezos-accuser-alpha man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > 007/tezos-accuser-007.html
    # generic
	@../tezos-admin-client man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > api/tezos-admin-client.html
	@../tezos-signer man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > api/tezos-signer.html
	@../tezos-snoop man -verbosity 3 -format html | sed "s#${HOME}#\$$HOME#g" > api/tezos-snoop.html

.PHONY: odoc
odoc: main
	@cd .. ; dune build @doc
	@rm -rf $$(pwd)/_build/api/odoc
	@cp -r $$(pwd)/../_build/default/_doc $$(pwd)/_build/api/odoc
	@echo '.toc {position: static}' >> $$(pwd)/_build/api/odoc/_html/odoc.css
	@echo '.content { margin-left: 4ex }' >> $$(pwd)/_build/api/odoc/_html/odoc.css
	@echo '@media (min-width: 745px) {.content {margin-left: 4ex}}' >> $$(pwd)/_build/api/odoc/_html/odoc.css
	@sed -e 's/@media only screen and (max-width: 95ex) {/@media only screen and (max-width: 744px) {/' $$(pwd)/_build/api/odoc/_html/odoc.css > $$(pwd)/_build/api/odoc/_html/odoc.css2
	@mv $$(pwd)/_build/api/odoc/_html/odoc.css2  $$(pwd)/_build/api/odoc/_html/odoc.css

linkcheck:
	$(SPHINXBUILD) -b linkcheck "$(SOURCEDIR)" "$(BUILDDIR)"

api/errors.rst: $(DOCERRORDIR)/error_doc.ml
	@cd .. && dune build docs/$(DOCERRORDIR)/error_doc.exe
	../_build/default/docs/$(DOCERRORDIR)/error_doc.exe > api/errors.rst

$(DOCGENDIR)/rpc_doc.exe:
	@cd .. && dune build docs/$(DOCGENDIR)/rpc_doc.exe

rpc: $(DOCGENDIR)/rpc_doc.exe
	@dune exec $(DOCGENDIR)/rpc_doc.exe "" > 007/rpc.rst
	@dune exec $(DOCGENDIR)/rpc_doc.exe 008 > 008/rpc.rst
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
	@-rm -Rf api/errors.rst 007/rpc.rst 008/rpc.rst shell/rpc.rst shell/p2p_api.rst
	@-rm -Rf api/tezos-*.html 007/tezos-*.html 008/tezos-*.html
