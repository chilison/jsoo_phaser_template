open Js_of_ocaml
open Firebug
open Js
open Level

class type config_map =
  object
    method data : int js_array t js_array t readonly_prop
    method tileWidth : int readonly_prop
    method tileHeight : int readonly_prop
  end

class type tileset = object end
class type dynamicLayer = object end

class type map =
  object
    method addTilesetImage :
      js_string t -> js_string t -> int -> int -> int -> int -> tileset t meth

    method createDynamicLayer :
      int -> tileset t -> int -> int -> dynamicLayer t meth

    method getTileAt : int -> int -> int
    method putTileAt : int -> int -> int -> unit meth
  end

class type game_object_creator =
  object
    method tilemap : config_map t -> map t meth
  end

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

let dungeon this =
  object%js (that)
    val sprites =
      object%js
        val floor = 0
        val wall = 554
      end

    method initialize () =
      console##log
        ((Js.Unsafe.eval_string {|obj => Object.keys(obj) |} : _ -> _) this);
      console##log "igxwelugler";
      console##log
        ((Js.Unsafe.eval_string {|obj => Object.keys(obj) |} : _ -> _) that);
      console##log "ukglcxkryditdji";
      let make : game_object_creator t =
        (Js.Unsafe.eval_string {|x => x.make |} : _ -> _) this
      in
      assert (Js.Optdef.test (Obj.magic make));
      console##log_2 (Js.string "make =  ") make;

      let level2 = Level.level2 in

      console##log_2 (Js.string "level =  ") level2;

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
      let (ground : dynamicLayer t) = _map##createDynamicLayer 0 _tileset 0 0 in
      console##log_2 (Js.string "ground =  ") ground;
      ground
  end
