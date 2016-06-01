
all:
	ocp-build init
	ocp-build build ocplib-resto ocplib-resto-directory

test:
	ocp-build init
	ocp-build build test
	./_obuild/test/test.byte

clean:
	[ ! -d _obuild ] || ocp-build clean

distclean:
	-rm -rf _obuild
	-rm -rf */*~ *~

