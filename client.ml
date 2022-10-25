(** Copyright 2022, Winnie Pooh *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Js_of_ocaml
open Firebug

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
    // https://photonstorm.github.io/phaser3-docs/Phaser.Types.Scenes.html#.CreateSceneFromObjectConfig
    const scene = {
      init: function () {
        console.log("scene.init", "this = ", this );
      },
      preload: function () {
        console.log("scene.preload", "this = ", this );
        console.log("this.load = ", this.load );

        this.load.bitmapFont("arcade", "assets/font/arcade.png", "assets/font/arcade.xml");
      },
      create: function () {
        console.log("scene.create", "this = ", this );
        this.add.bitmapText(400, 300, "arcade", "Hello Phaser").setOrigin(0.5);
      }
    };
    scene; // last value is a result of expression
|}

open Js

(** See also: https://photonstorm.github.io/phaser3-docs/Phaser.Loader.LoaderPlugin.html *)
class type loader_plugin =
  object
    method bitmapFont : js_string t -> js_string t -> js_string t -> unit meth
  end

(* TODO: understand what an object it is, and which class name is desirable *)
class type o =
  object
    method setOrigin : float -> unit meth
  end

(* TODO: understand what an object it is, and which class name is desirable *)
class type xxxx =
  object
    method bitmapText : int -> int -> js_string t -> js_string t -> o t meth
  end

(* It looks like we are constructing this object here:
   https://photonstorm.github.io/phaser3-docs/Phaser.Types.Scenes.html#.CreateSceneFromObjectConfig__anchor *)
let caml_scene =
  (* TODO: in the line below it gives a warning about unused variable 'this'. No idea why, it shouldn't *)
  object%js (this)
    method init = ()

    method preload =
      (* In two functions below Javascript expects (I think) that a BIG object will be extended
         by fields of small one (which is below). It seems to be a common pattern in JS worls,
         but not in the statically typed one. There is high change that unsafe hacks will be
         really needed. But it's worth investigating *)
      Firebug.console##log (Js.string "preload");
      console##log_2 (Js.string "self = ") this;
      let loader : loader_plugin t =
        (Js.Unsafe.eval_string {|x => x.load |} : _ -> _) this
      in
      console##log_2 (Js.string "laod =  ") loader;
      loader##bitmapFont (Js.string "arcade")
        (Js.string "assets/font/arcade.png")
        (Js.string "assets/font/arcade.xml")

    method create () =
      Firebug.console##log (Js.string "Create");
      console##log_2 (Js.string "self = ") this;
      let add : xxxx t =
        (Js.Unsafe.eval_string {|x => x.add |} : _ -> _) this
      in
      console##log_2 (Js.string "add =  ") add;
      let (o : o t) =
        add##bitmapText 400 300 (Js.string "arcade") (Js.string "Hello Phaser")
      in
      o##setOrigin 0.5

    (* Long term recommendation is to design OCaml API in a manner that game developer will not need to do any unsafe hacks *)
  end

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
    val scene = caml_scene

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

let _game = new%js make_game config
