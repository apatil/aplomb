.PHONY: byte test native clean install assets

all : build

# First you need to npm install babel and set it up to convert to es5 as documented
# here: https://stackoverflow.com/questions/34747693/how-do-i-get-babel-6-to-compile-to-es5-javascript

assets/vega.js:
	babel assets/vega-es6.js --out-file assets/vega.js

assets/vega-embed.js:
	babel assets/vega-embed-es6.js --out-file assets/vega-embed.js

assets/vega-lite.js:
	babel assets/vega-lite-es6.js --out-file assets/vega-lite.js

vegaLiteAssets.ml:
	ocaml-crunch -m plain -o vegaLiteAssets.ml assets

assets: assets/vega.js assets/vega-embed.js assets/vega-lite.js vegaLiteAssets.ml

byte:
	ocamlbuild -use-ocamlfind -pkgs ppx_tools,ppx_deriving,ppx_tools.metaquot ppx_deriving_aplomb.cma
	ocamlbuild -use-ocamlfind -pkgs yojson,vega-lite aplomb.cma
	ocamlbuild -use-ocamlfind -pkgs yojson,markup,webview,vega-lite viewCommon.cma
	ocamlbuild -use-ocamlfind -tag thread -pkgs str,yojson,markup,webview,vega-lite,unix,uri,webbrowser aplombLocal.cma

native:
	ocamlbuild -use-ocamlfind -pkgs ppx_tools,ppx_deriving,ppx_tools.metaquot ppx_deriving_aplomb.cmxa
	ocamlbuild -use-ocamlfind -pkgs yojson,vega-lite aplomb.cmxa
	ocamlbuild -use-ocamlfind -pkgs yojson,markup,webview,vega-lite viewCommon.cmxa
	ocamlbuild -use-ocamlfind -tag thread -pkgs str,yojson,markup,webview,vega-lite,unix,uri,webbrowser aplombLocal.cmxa

build : byte native

install : build
	ocamlfind install aplomb META _build/*.cm*

clean:
	rm -rf _build

test:
	ocamlbuild -use-ocamlfind -tag thread -pkgs yojson,vega-lite,webview,markup,str,unix,threads,uri,webbrowser test.byte --
