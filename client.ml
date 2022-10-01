(** Copyright 2022, Winnie Pooh *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Js_of_ocaml
open Firebug
open Js_of_ocaml.WebSockets

let () = assert (WebSockets.is_supported ())

let () =
  let sock = new%js webSocket (Js.string "ws://localhost:3000") in
  sock##.onopen :=
    Dom.handler (fun _e ->
        console##log (Js.string "opened");
        sock##send (Js.string "ping");
        Js._true);

  sock##.onmessage :=
    Dom.handler (fun e ->
        console##log_2 (Js.string "onmessage:") e##.data;
        Js._true)

(* Dirty hack. OCaml library fo rphaser should be constructed and used type safely *)
let _ =
  Js.Unsafe.eval_string
    {|
  const scene = {
    preload: function () {
      this.load.bitmapFont("arcade", "assets/font/arcade.png", "assets/font/arcade.xml");
    },
    create: function () {
      this.add.bitmapText(400, 300, "arcade", "Hello Phaser").setOrigin(0.5);
    }
  }
  const config = {
    type: Phaser.AUTO,
    width: 800,
    height: 600,
    backgroundColor: "#000",
    parent: "game",
    pixelArt: true,
    scene: scene,
    physics: {
      default: "arcade",
      arcade: {
        gravity: { y: 0 }
      }
    }
  };
  const game = new Phaser.Game(config);
|}
