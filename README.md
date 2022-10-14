#### Client/server js_of_ocaml example


* Building: `dune build @default`
* Running: `dune exec ./server.exe`
  It will print that client is available at `http://localhost:8888/`



#### Building dependencies

`opam install  yojson cohttp lwt websocket-lwt-unix lwt_ppx js_of_ocaml-ppx ppx_inline_test --yes`

#### IDE integration

Is as usual as for every OCaml project

`opam install ocaml-lsp-server ocamlformat`

#### Useful links

* https://photonstorm.github.io/phaser3-docs/
* https://ocsigen.org/js_of_ocaml/latest/manual/bindings
* https://ocsigen.org/js_of_ocaml/latest/manual/ppx
