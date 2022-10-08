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

(* Dirty hack. OCaml library for phaser should be constructed and used type safely.
   Below there is an attempt to rewrite it in more type-safe OCaml-like way.
*)
let __ () =
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

(* Rewriting scene is not yet done *)
let scene =
  Js.Unsafe.eval_string
    {|
    const scene = {
      preload: function () {
        //console.log("scene.preload");
        this.load.bitmapFont("arcade", "assets/font/arcade.png", "assets/font/arcade.xml");
      },
      create: function () {
        //console.log("scene.create");
        this.add.bitmapText(400, 300, "arcade", "Hello Phaser").setOrigin(0.5);
      }
    };
    scene; // last value is a result of expression
|}

type game

let make_game : ('config -> game Js.t) Js.constr =
  Js.Unsafe.global##._Phaser##._Game

let config =
  object%js (self)
    val type_ = Js.Unsafe.eval_string {| Phaser.AUTO |}
    val width = 800
    val height = 600
    val backgroundColor = Js.string "#000"
    val parent = Js.string "game"
    val pixelArt = Js._true
    val scene = scene

    val physics =
      object%js
        val default = Js.string "arcade"

        val arcade =
          object%js
            val gravity =
              object%js
                val y = 0
              end
          end
      end
  end

(* let () = Firebug.console##log config *)
let _game = new%js make_game config
