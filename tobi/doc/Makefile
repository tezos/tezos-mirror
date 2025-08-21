COMPONENT_NAME=Tobi
COMPONENT_EXE=../../_build/default/tobi/src/main.exe
MD_FILES:=$(wildcard src/*.md)

# Define a variable that contains a space.
# Needed because we can't write spaces directly in $(subst).
space:=' '

.PHONY: all
all: all-but-help help

.PHONY: help
help: html/tobi--help.html \
	html/tobi_list--help.html \
	html/tobi_install--help.html \
	html/tobi_reset--help.html \
	html/tobi_build--help.html

.PHONY: all-but-help
all-but-help: $(patsubst src/%.md,html/%.html,$(MD_FILES)) html/style.css

# All rules that generate files in the 'html' directory depend on the 'html' directory.
# Those dependencies must be of the form '| html', meaning "ignore the timestamp".
# A normal dependency would not work because of the way directory timestamps change.
html:
	mkdir html

# To generate an html file from a markdown file, we call 'markdown',
# which must be installed (e.g. with 'sudo apt install markdown').
#
# We also insert some common headers and footers.
# We generate a title from the component name and the filename,
# by replacing underscores with spaces.
#
# We also replace URLs of the form X.md with X.html.
# This allows links in the markdown version to link to the markdown version,
# and links in the HTML version to link to the HTML version.
html/%.html: src/%.md src/header1.html src/header2.html src/footer.html | html
	cat src/header1.html > $@
	echo '$(COMPONENT_NAME): $(subst _,$(space),$*)' >> $@
	cat src/header2.html >> $@
	sed 's/\.md)/\.html)/g' src/$*.md | markdown >> $@
	cat src/footer.html >> $@

# Copy src/style.css into html/style.css.
# We could instead commit the file directly into html/,
# but being able to 'rm -rf html' is convenient.
html/style.css: src/style.css | html
	cp $< $@

# The executable needs to be rebuilt if its source changes,
# so that the help pages are up-to-date.
$(COMPONENT_EXE): $(wildcard ../src/*.ml*)
	dune build ../src/main.exe

# When generating help pages we set TERM to dumb to tell Clap to disable ANSI escape codes.
html/tobi--help.txt: $(COMPONENT_EXE) | html
	TERM=dumb $(COMPONENT_EXE) --help 2> $@

html/tobi_%--help.txt: $(COMPONENT_EXE) | html
	TERM=dumb $(COMPONENT_EXE) $* --help 2> $@

# To generate an html file from a help text file, we simply wrap it under
# headers and footers, and under a <pre>. We also add a title.
html/%--help.html: html/%--help.txt src/header1.html src/header2.html src/footer.html
	cat src/header1.html > $@
	echo '$(subst _,$(space),$*) --help' >> $@
	cat src/header2.html >> $@
	echo -n '<h1><code>$(subst _,$(space),$*) --help</code></h1><pre>' >> $@
	cat $< >> $@
	echo '</pre>' >> $@
	cat src/footer.html >> $@
