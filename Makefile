.PHONY: build test examples install uninstall reinstall clean

FINDLIB_NAME=osx-fsevents
MOD_NAME=fsevents

OCAML_LIB_DIR=$(shell ocamlc -where)

CTYPES_LIB_DIR=$(shell ocamlfind query ctypes)

OCAMLBUILD=CTYPES_LIB_DIR=$(CTYPES_LIB_DIR) OCAML_LIB_DIR=$(OCAML_LIB_DIR) \
	ocamlbuild -use-ocamlfind -classic-display

WITH_LWT=$(shell ocamlfind query lwt > /dev/null 2>&1 ; echo $$?)

TARGETS=.cma .cmxa .cmx

PRODUCTS=$(addprefix $(MOD_NAME),$(TARGETS)) \
	lib$(MOD_NAME)_stubs.a dll$(MOD_NAME)_stubs.so

ifeq ($(WITH_LWT), 0)
PRODUCTS+=$(addprefix $(MOD_NAME)_lwt,$(TARGETS)) fs_events.native
endif

TYPES=.mli .cmi .cmti

INSTALL:=$(addprefix $(MOD_NAME), $(TYPES)) \
         $(addprefix $(MOD_NAME), $(TARGETS))

INSTALL:=$(addprefix _build/lib/,$(INSTALL))

ifeq ($(WITH_LWT), 0)
INSTALL_LWT:=$(addprefix $(MOD_NAME)_lwt, $(TYPES)) \
             $(addprefix $(MOD_NAME)_lwt, $(TARGETS))

INSTALL_LWT:=$(addprefix _build/lwt/,$(INSTALL_LWT))

INSTALL+=$(INSTALL_LWT)
endif

ARCHIVES:=_build/lib/$(MOD_NAME).a

ifeq ($(WITH_LWT), 0)
ARCHIVES+=_build/lwt/$(MOD_NAME)_lwt.a
endif

build:
	$(OCAMLBUILD) $(PRODUCTS)

test: build
	$(OCAMLBUILD) lib_test/test.native
	./test.native

examples: build
	$(OCAMLBUILD) examples/print_events.native

install:
	ocamlfind install $(FINDLIB_NAME) META \
		$(INSTALL) \
		-dll _build/lib/dll$(MOD_NAME)_stubs.so \
		-nodll _build/lib/lib$(MOD_NAME)_stubs.a \
		$(ARCHIVES)

uninstall:
	ocamlfind remove $(FINDLIB_NAME)

reinstall: uninstall install

clean:
	ocamlbuild -clean
	rm -f lib/fsevents.cm? lib/fsevents.o
