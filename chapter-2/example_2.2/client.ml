(** Copyright 2022, Winnie Pooh *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Js_of_ocaml
open Firebug
open Js

class type loader_plugin =
  object
    method bitmapFont : js_string t -> js_string t -> js_string t -> unit meth
  end

class type game_object =
object
  method destroy : unit -> unit meth
  (* method active : bool prop
  method visible : bool prop *)
end

class type bitmap_text =
  object
  inherit game_object
    method x : int prop
    method y : int prop
    method font : string prop
    method text : string prop
    method setOrigin : float -> unit meth
  end

class type game_object_creator =
  object
    method bitmapText :
      int -> int -> js_string t -> js_string t -> bitmap_text t meth
  end

let helloText : bitmap_text Js.t ref = ref (Js.Unsafe.js_expr "1")

let caml_scene =
  object%js (this)
    method init = ()

    method preload =
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
      let add : game_object_creator t =
        (Js.Unsafe.eval_string {|x => x.add |} : _ -> _) this
      in
      console##log_2 (Js.string "add =  ") add;
      let hello2Text =
        add##bitmapText 400 300 (Js.string "arcade") (Js.string "Hello Phaser")
      in

      helloText := hello2Text;
      hello2Text##setOrigin 0.5

    method update () =
      Firebug.console##log (Js.string "Update");

      !helloText##.x := !helloText##.x + 10;
      
      if !helloText##.x > 600 then (!helloText##.x := 0; console##log (Js.string "helloText moved"));
        if !helloText##.x == 0 then (!helloText##destroy (); console##log (Js.string "helloText dead"))
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