(** Copyright 2022, Winnie Pooh *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Js_of_ocaml
open Firebug
open Js
open Dungeon
open TurnManager
open Player

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
      let player = Player.make_player this 15 15;
      let entityPlayer : entity Js.t ref = ref (Js.Unsafe.js_expr "1") in
      a;

      (*Entity is expected and player is not compatible with this type. It definitely works somehow in JavaScript version.
        I suppose, I'll have to get rid of entity type or create player and its entity clone to copy the values ​​of the required fields,
        it might lead to new errors though*)
      TurnManager.tm##addEntity !entityPlayer !entityPlayer##.x := player##.x;
      !entityPlayer##.y := player##.y;
      !entityPlayer##.movementPoints := player##.movementPoints;
      !entityPlayer##.sprite := player##.sprite

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
