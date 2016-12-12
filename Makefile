all: spacetime_lib

spacetime_lib:
	$(MAKE) -C src

clean:
	$(MAKE) -C src $@

.PHONY: all spacetime_lib clean install uninstall reinstall

DIST_FILES=\
	src/spacetime_lib.mli	\
	src/spacetime_lib.ml    \
	src/spacetime_lib.cmi   \
	src/spacetime_lib.cmx	\
	src/spacetime_lib.a	\
	src/spacetime_lib.cmxa

$(DIST_FILES): spacetime_lib

install: $(DIST_FILES) src/META
	ocamlfind install spacetime_lib $^

uninstall:
	ocamlfind remove spacetime_lib

reinstall:
	-$(MAKE) uninstall
	$(MAKE) install
