KSYS:=$(shell find ./files -name \*.ksy -print)
HTMLS:=$(patsubst %.ksy,doc/%.html,$(KSYS))

all: $(HTMLS)

doc/:
	mkdir -p doc/

doc/%.html: %.ksy doc/
	kaitai-struct-compiler -t html $< --outdir=doc/
