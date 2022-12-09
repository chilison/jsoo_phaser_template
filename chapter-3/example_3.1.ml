(** Copyright 2022, Winnie Pooh *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Js_of_ocaml
open Firebug
open Js

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

class type config_map =
  object
    method data : int js_array t js_array t readonly_prop
    method tileWidth : int readonly_prop
    method tileHeight : int readonly_prop
  end

class type tileset = object end
class type staticLayer = object end

class type map =
  object
    method addTilesetImage :
      js_string t -> js_string t -> int -> int -> int -> int -> tileset t meth

    method createStaticLayer :
      int -> tileset t -> int -> int -> staticLayer t meth
  end

class type game_object_creator =
  object
    method tilemap : config_map t -> map t meth
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

      let make : game_object_creator t =
        (Js.Unsafe.eval_string {|x => x.make |} : _ -> _) this
      in
      console##log_2 (Js.string "make =  ") make;

      let level =
        [|
          Js.array [| 554; 554; 554; 554; 554; 554; 554; 554; 554; 554 |];
          Js.array [| 554; 0; 0; 0; 0; 0; 0; 0; 0; 554 |];
          Js.array [| 554; 0; 0; 0; 0; 0; 0; 0; 0; 554 |];
          Js.array [| 554; 0; 0; 0; 0; 0; 0; 0; 0; 554 |];
          Js.array [| 554; 0; 0; 0; 0; 0; 0; 0; 0; 554 |];
          Js.array [| 554; 0; 0; 0; 0; 0; 0; 0; 0; 554 |];
          Js.array [| 554; 0; 0; 0; 0; 0; 0; 0; 0; 554 |];
          Js.array [| 554; 0; 0; 0; 0; 0; 0; 0; 0; 554 |];
          Js.array [| 554; 0; 0; 0; 0; 0; 0; 0; 0; 554 |];
          Js.array [| 554; 554; 554; 554; 554; 554; 554; 554; 554; 554 |];
        |]
      in
      let level2 = Js.array level in

      console##log_2 (Js.string "level =  ") level;
      let tileSize = 16 in

      let current_map_config : config_map Js.t =
        object%js (self)
          val data = level2
          val tileWidth = 16
          val tileHeight = 16
        end
      in
      console##log_2
        (Js.string "current_map_config##.data =  ")
        current_map_config##.data;
      let (_map : map t) = make##tilemap current_map_config in
      let (_tileset : tileset t) =
        _map##addTilesetImage (Js.string "tiles") (Js.string "tiles") tileSize
          tileSize 0 1
      in
      console##log_2 (Js.string "_tileset =  ") _tileset;
      let (ground : staticLayer t) = _map##createStaticLayer 0 _tileset 0 0 in
      console##log_2 (Js.string "ground =  ") ground;
      ground
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
    val zoom = 2
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
