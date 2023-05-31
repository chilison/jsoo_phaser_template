open Js_of_ocaml
open Js
open Level
open Entity
open TurnManager
open Libgame.Phaser_bindings
open Libgame.Pathfinding_bindings

let curr_map : map Js.t ref = ref (Js.Unsafe.js_expr "1")
let player_character : entity ref = ref (Js.Unsafe.js_expr "2")

type 'entity check_ent = 'entity option = None | Some of 'entity
type 'int check_path = 'int option = None | Some of 'int

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
      let level2 = Level.level2 
      in
      let current_map_config : config_map Js.t =
        object%js 
          val data = level2
          val tileWidth = that##.tileSize
          val tileHeight = that##.tileSize
        end
      in
      let (_map2 : map t) = make##tilemap current_map_config in
      curr_map := _map2;
      let (_tileset : tileset t) =
        !curr_map##addTilesetImage (Js.string "tiles") (Js.string "tiles")
          that##.tileSize that##.tileSize 0 1
      in
      let (ground : dynamicLayer t) =
        !curr_map##createDynamicLayer 0 _tileset 0 0
      in
      ground

    method isWalkableTile : int -> int -> bool =
      fun x y ->
      let allEntities = TurnManager.tm##.entities in
        let flag : bool = SS.fold (fun ent flag  -> if (ent#get_x == x && ent#get_y == y || flag == false) then false else true) allEntities true in 
            if flag = false then false else
      let tileAtDestination = !(curr_map)##getTileAt x y in
      tileAtDestination##.index <> that##.sprites##.wall

      method entityAtTile : int -> int -> check_ent =
        fun x y -> 
          let allEntities = TurnManager.tm##.entities in
          let default_entity = new entity in
          let acc : entity  = SS.fold (fun ent acc  -> if (ent#get_x == x && ent#get_y == y) then ent else acc) allEntities default_entity in 
          if acc <> default_entity then Some acc else
        None

      method removeEntity : entity t -> unit =
        fun entity  ->
          TurnManager.tm##removeEntity entity;  
          entity#get_sprite##destroy ();
          entity#onDestroy ()
          
          
        
      method moveEntityTo : 'a 'b. twist -> 'b -> int -> int -> unit =
        fun twist entity xx yy ->
          entity#set_moving true;
          entity#set_x xx;
          entity#set_y yy;
        
          let tweens : tween_manager t =
            (Js.Unsafe.eval_string {|x => x.tweens |} : _ -> _) twist
          in
          let tweens_builder_config : tweensBuilderConfig Js.t =
            object%js
              val targets : sprite t = entity#get_sprite
              val onComplete = fun () ->
              entity#set_moving false
              val x = !curr_map##tileToWorldX xx
              val y = !curr_map##tileToWorldX yy
              val ease = Js.string "Power2"
              val duration = 200
              val delay = 0
              val hold = 0
              val yoyo = false
            end
          in
          tweens##add tweens_builder_config

      method distanceBetweenEntities : entity t -> entity t -> check_path =
        fun e1 e2 ->
          let finder_config =
            object%js
              val allowDiagonal : bool = true
            end
          in
          let make_grid : (int js_array t js_array t -> grid t) Js.constr =
            Js.Unsafe.global##._PF##._Grid
          in
          let make_finder : ('finder_config -> finder t) Js.constr =
            Js.Unsafe.global##._PF##._AStarFinder
          in
          let finder = new%js make_finder finder_config in
          let grid = new%js make_grid level2 in
          let path = finder##findPath e1#get_x e1#get_y e2#get_x e2#get_y grid in
          if path##.length >= 2 then Some path##.length else None

      method attackEntity : entity -> entity -> unit =
        fun attacker victim -> 
          attacker#set_moving true;
          attacker#set_tweens (attacker#get_tweens + 1);

          let tweens : tween_manager t =
            (Js.Unsafe.eval_string {|x => x.tweens |} : _ -> _) twist
          in
          let tweens_builder_config : tweensBuilderConfig Js.t =
            object%js (this)
              val targets : sprite t = attacker#get_sprite

              val onComplete = fun () ->
                attacker#get_sprite##.x := !curr_map##tileToWorldX attacker#get_x;
                attacker#get_sprite##.y := !curr_map##tileToWorldX attacker#get_y;
                attacker#set_moving false;
                attacker#set_tweens (attacker#get_tweens - 1);
                
                let damage = attacker#attack  in
                victim#set_healthPoints (victim#get_healthPoints - damage); 
                if victim#get_healthPoints <= 0 then (
                  that##removeEntity victim;
                  (*another way to delete the entity the memory is leaking though*)
                  (* victim#get_sprite##.active := false;
                  victim#get_sprite##.visible := false *)
                )
              
              val x = !curr_map##tileToWorldX victim#get_x
              val y = !curr_map##tileToWorldX victim#get_y
              val ease = Js.string "Power2"
              val duration = 80
              val hold = 20
              val delay = attacker#get_tweens * 200
              val yoyo = true
            end
          in
          tweens##add tweens_builder_config          
  end

