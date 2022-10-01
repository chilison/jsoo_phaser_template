#### Client/server js_of_ocaml example


* Building: `dune build @default`
* Running: `dune exec ./server.exe`
  It will print that client is available at `http://localhost:8888/`



#### Building dependencies

`opam install  yojson cohttp lwt websocket-lwt-unix lwt_ppx js_of_ocaml-ppx`

#### IDE integration

Is as usual as for every OCaml project

`opam install ocaml-lsp-server ocamlformat`
