.PHONY: all release deps

all:
	dune b @default

release:
	dune b @default --profile=release


deps:
	opam install yojson cohttp lwt websocket-lwt-unix lwt_ppx js_of_ocaml-ppx ppx_inline_test --yes --depext