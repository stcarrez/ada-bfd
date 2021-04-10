NAME=bfdada

-include Makefile.conf

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XBFDADA_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XBFDADA_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable

include Makefile.defaults

# Build executables for all mains defined by the project.
build-test::	setup
	$(GNATMAKE) $(GPRFLAGS) -p -P$(NAME)_tests $(MAKE_ARGS)

# Build and run the unit tests
test:	build build-test samples
	bin/bfdada_harness
	@sh ./check-samples.sh

install-samples:
	$(MKDIR) -p $(samplesdir)/samples
	cp -rp $(srcdir)/samples/*.ad[sb] $(samplesdir)/samples/
	cp -p $(srcdir)/samples.gpr $(samplesdir)
	cp -p $(srcdir)/config.gpr $(samplesdir)

$(eval $(call ada_library,$(NAME)))

check:	test

samples:	force
	$(GNATMAKE) $(GPRFLAGS) -p -Psamples $(MAKE_ARGS)

force:

setup:: src/bfd-constants.ads

src/bfd-constants.ads:	bin/bfdgen
	bin/bfdgen > $@

# Utility for the generation of bfd-constants.ads
# Build this program with -g so that it is used by the unit tests to check some BFD flags.
bin/bfdgen:    support/bfdgen.c
	mkdir -p bin
	$(CC) -o $@ $(CFLAGS) -g support/bfdgen.c

clean::
	rm -f src/bfd-constants.ads
