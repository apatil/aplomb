.PHONY: all build byte test native clean install

all : build

byte:
	ocamlbuild -use-ocamlfind -pkgs ppx_tools,ppx_deriving,ppx_tools.metaquot ppx_deriving_aplomb.cma
	ocamlbuild -use-ocamlfind -pkgs yojson,vega-lite aplomb.cma

native:
	ocamlbuild -use-ocamlfind -pkgs ppx_tools,ppx_deriving,ppx_tools.metaquot ppx_deriving_aplomb.cmxa
	ocamlbuild -use-ocamlfind -pkgs yojson,vega-lite aplomb.cmxa

build : byte native

install : build
	ocamlfind install aplomb META _build/*.cm*

clean:
	rm -rf _build
