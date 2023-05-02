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

class type tile =
  object
    method index : int readonly_prop
  end

class type map =
  object
    method addTilesetImage :
      js_string t -> js_string t -> int -> int -> int -> int -> tileset t meth

    method createDynamicLayer :
      int -> tileset t -> int -> int -> dynamicLayer t meth

    method getTileAt : int -> int -> tile t meth
    method putTileAt : int -> int -> int -> unit meth
    method tileToWorldX : int -> int meth
    method tileToWorldY : int -> int meth
  end

class type sprite =
  object
    method tint : int prop
    method no : int prop
    method setOrigin : float -> unit meth
  end

class type game_object_creator =
  object
    method tilemap : config_map t -> map t meth
    method sprite : int -> int -> js_string t -> int -> sprite t meth
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

class type tweenBuilderConfig =
  object
    method targets : sprite t readonly_prop
    method onComplete : unit meth
    method x : int readonly_prop
    method y : int readonly_prop
    method ease : js_string t readonly_prop
    method duration : int readonly_prop
  end

class type tween_manager =
  object
    method add : tweenBuilderConfig t -> unit meth
  end


let curr_map : map Js.t ref = ref (Js.Unsafe.js_expr "1")
let player_character : Entity.entity ref = ref (Js.Unsafe.js_expr "2")

let dungeon twist =
  object%js (that)
    val sprites =
      object%js
        val floor = 0
        val wall = 554
      end

    val tileSize = 16

    method initialize () =
      let make : game_object_creator t =
        (Js.Unsafe.eval_string {|x => x.make |} : _ -> _) twist
      in
      assert (Js.Optdef.test (Obj.magic make));
      console##log_2 (Js.string "make =  ") make;

      let level2 = Level.level2 in

      console##log_2 (Js.string "level =  ") level2;

      let current_map_config : config_map Js.t =
        object%js (self)
          val data = level2
          val tileWidth = that##.tileSize
          val tileHeight = that##.tileSize
        end
      in
      console##log_2
        (Js.string "current_map_config##.data =  ")
        current_map_config##.data;
      let (_map2 : map t) = make##tilemap current_map_config in
      curr_map := _map2;
      let (_tileset : tileset t) =
        !curr_map##addTilesetImage (Js.string "tiles") (Js.string "tiles")
          that##.tileSize that##.tileSize 0 1
      in
      console##log_2 (Js.string "_tileset =  ") _tileset;
      let (ground : dynamicLayer t) =
        !curr_map##createDynamicLayer 0 _tileset 0 0
      in
      console##log_2 (Js.string "ground =  ") ground;
      ground

    method isWalkableTile x y =
      let ( let* ) = Optdef.bind in
      let* arr = array_get level2 y in
      let* axy = array_get arr x in
      Optdef.return (axy <> 554)

    (* `TODO: get rid of optedef /???? *)

    (* method initializeEntity : 'a. 'a -> unit =
       fun this ->
         let add : game_object_creator t =
           (Js.Unsafe.eval_string {|x => x.add |} : _ -> _) this
         in

         let x = !curr_map##tileToWorldX this##.x in
         let y = !curr_map##tileToWorldX this##.y in
         this##.sprite := add##sprite x y (Js.string "tiles") this##.tile;
         console##log (Js.string "in initializeEntity");
         this##.sprite##setOrigin 0 *)

    method moveEntityTo : 'a 'b. twist -> 'b -> int -> int -> unit =
      fun twist entity xx yy ->
        entity#set_moving true;
        let tweens : tween_manager t =
          (Js.Unsafe.eval_string {|x => x.tweens |} : _ -> _) twist
        in
        let tween_builder_config : tweenBuilderConfig Js.t =
          object%js
            val targets : sprite t = entity#get_sprite

            method onComplete =
              entity#set_moving false;
              entity#set_x xx;
              entity#set_y yy

            val x = !curr_map##tileToWorldX xx
            val y = !curr_map##tileToWorldX yy
            val ease = Js.string "Power2"
            val duration = 200
          end
        in
        tweens##add tween_builder_config
  end

