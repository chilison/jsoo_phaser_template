open Js_of_ocaml
open Js
open Firebug
open Entity
open Dungeon

type grid

class type path =
  object
    method length : int prop
    
  end

class type finder =
  object
    method findPath : int -> int -> int -> int -> grid t -> path t meth
  end
  
class type key =
  object
    method isDown : bool prop
  end

class type cursor_keys =
  object
    method up : key t prop
    method down : key t prop
    method right : key t prop
    method left : key t prop
  end

class type keyboard_creator =
  object
    method createCursorKeys : unit -> cursor_keys t meth
  end

class type input_plugin =
  object
    method keyboard : keyboard_creator t prop
  end

let cursors : cursor_keys Js.t ref = ref (Js.Unsafe.js_expr "1")

  let path_get : path t -> int -> int -> int =
    fun path i j ->
      let arr: int  js_array Js.t  js_array Js.t =
      Js.Unsafe.coerce (path) in 
      (
      let ( let* ) = Optdef.bind in
      let* arrr = array_get arr i in
      let* x = array_get arrr j in Optdef.return x  )
      |>  Obj.magic 



class basicMonster =
  object (self : 'self)
    inherit Entity.entity

    method! make : 'a. 'a -> int -> int -> unit =
      fun this newX newY -> 
        name <- "The basic monster";
        movementPoints <- 1;
        actionPoints <- 3;
        healthPoints <- 1;
        x <- newX;
        y <- newY;
        movementPoints <- 1;
        tile <- 26;
        let add : game_object_creator t =
          (Js.Unsafe.eval_string {|x => x.add |} : _ -> _) this
        in
        let xx = !curr_map##tileToWorldX x in
        let yy = !curr_map##tileToWorldX y in
        sprite <- add##sprite xx yy (Js.string "tiles") tile;
        sprite##setOrigin 0.

        method! turnChar : twist t -> unit =
          fun twist ->

            let make_starfinder : (unit -> finder Js.t) Js.constr =
              Js.Unsafe.global##._PF##._AStarFinder
            in
            let make_grid : (int js_array t js_array t -> grid Js.t) Js.constr =
              Js.Unsafe.global##._PF##._Grid
            in

            let oldX : int = x in
                let  oldY : int = y
            in
            let pX : int = !Dungeon.player_character#get_x in
            let  pY : int = !Dungeon.player_character#get_y in

            let grid = new%js make_grid Level.level2 
            in
            let  finder = new%js make_starfinder ()
            in
            let path = finder##findPath oldX oldY pX pY grid
            in
              if movementPoints > 0 then (
                if path##.length > 2 then (
                  (Dungeon.dungeon twist)##moveEntityTo
                     twist self
                     (path_get path 1 0)
                     (path_get path 1 1));
                  movementPoints <- movementPoints - 1);
              if actionPoints > 0 then (
                if Option.get ((Dungeon.dungeon twist)##distanceBetweenEntities self !Dungeon.player_character) <= 2
                   then (Dungeon.dungeon twist)##attackEntity self !Dungeon.player_character;
                actionPoints <- actionPoints - 1)    
  end