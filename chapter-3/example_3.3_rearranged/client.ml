(** Copyright 2022, Winnie Pooh *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Js_of_ocaml
open Firebug
open Js
open Dungeon
open TurnManager

(* This is a different version of example_3.3 which is more likely to work out.
   What exactly I did: got rid of entity class, made character class instead, divided make_player and
   set_player (maybe I`ll have to bring them back together when 'this' problem (lower) is solved), player module
   is temporarily glued to turnManager module 'cos some cycle relationships suddenly occured. *)

class type spritesheetConfig =
  object
    method frameWidth : int readonly_prop
    method frameHeight : int readonly_prop
    method spacing : int readonly_prop
  end

class type loader_plugin =
  object
    method spritesheet :
      js_string t -> js_string t -> spritesheetConfig t -> unit meth
  end

let caml_scene =
  object%js (this)
    method init = ()

    method preload =
      Firebug.console##log (Js.string "preload");
      console##log_2 (Js.string "self = ") this;
      let loader : loader_plugin t =
        (Js.Unsafe.eval_string {|x => x.load |} : _ -> _) this
      in
      (* Js.Optdef. *)
      (* assert (Js.Optdef.test (Js.Unsafe.coerce loader)); *)
      console##log_2 (Js.string "load =  ") loader;

      let spritesheet_config : spritesheetConfig Js.t =
        object%js (self)
          val frameWidth = 16
          val frameHeight = 16
          val spacing = 1
        end
      in

      loader##spritesheet (Js.string "tiles")
        (Js.string "assets/colored.png")
        spritesheet_config

    method create () =
      Firebug.console##log (Js.string "create");
      console##log_2 (Js.string "self = ") this;
      let a = (Dungeon.dungeon this)##initialize () in
      console##log
        ((Js.Unsafe.eval_string {|obj => Object.keys(obj) |} : _ -> _) this);
      Firebug.console##log (Js.string "player");
      let player = new character in
      player#set_player 15 15;
      (* I don`t know how to pass 'this' as an argument to the method, i tried to do it another way by
         making make_player a separate function but then I have no idea how to pass player as an arguement.
         I left this piece of code as I want it to be.. sooooo it doesn`t work :/*)
      player#make_player this;

      TurnManager.tm##addEntity player;
      a

    method update () =
      if TurnManager.tm##over () == true then TurnManager.tm##refresh ();
      TurnManager.tm##turn ()
  end

type game

let make_game : ('config -> game Js.t) Js.constr =
  Js.Unsafe.global##._Phaser##._Game

let config =
  object%js (self)
    val type_ = Js.Unsafe.eval_string {| Phaser.AUTO |}
    val width = 80 * 16
    val height = 50 * 16
    val backgroundColor = Js.string "#000"
    val parent = Js.string "game"
    val pixelArt = Js._true
    val zoom = 1
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
