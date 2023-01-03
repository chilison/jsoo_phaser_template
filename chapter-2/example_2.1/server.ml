(** Copyright 2022, Winnie Pooh *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let string_of_client client =
  client |> Ws.Client.id |> Ws.Client.Id.to_int |> string_of_int

let on_connect client =
  Printf.printf "client %s connected\n%!" (string_of_client client);
  Lwt.return ()

let classify_cmd _ = `Ping

let handler client message =
  Format.printf "Message got: %s\n%!" message;
  let sk msg = Ws.Client.send client (Yojson.Safe.to_string msg) in

  match classify_cmd message with _ -> sk (`String "Pong")

type cfg = {
  mutable prefix : string;
  mutable http_port : int;
  mutable ws_port : int;
}

let cfg = { prefix = "_build/default"; http_port = 8888; ws_port = 3000 }

open Printf

let http_server =
  let open Cohttp_lwt_unix in
  let callback _conn req _body =
    let respond fname = Server.respond_file ~fname () in
    match Request.resource req with
    | "/" -> respond (cfg.prefix ^ "/index.html")
    | s -> respond (cfg.prefix ^ s)
  in

  printf "Creating http server on http://localhost:%d/ ...\n%!" cfg.http_port;
  Server.create ~mode:(`TCP (`Port cfg.http_port)) (Server.make ~callback ())

let () =
  let open Lwt in
  printf "Creating WebSocket   server on port %d ...\n%!" cfg.ws_port;
  let server = Ws.Server.create ~port:cfg.ws_port in
  Lwt_main.run (Ws.Server.run ~on_connect server handler <&> http_server)
