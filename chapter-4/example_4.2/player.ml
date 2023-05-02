open Js_of_ocaml
open Firebug
open Js
open Dungeon
open Entity

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

class type twist = object end

let cursors : cursor_keys Js.t ref = ref (Js.Unsafe.js_expr "1")
let spr : sprite Js.t ref = ref (Js.Unsafe.js_expr "2")

class character =
  object (self : 'self)
    inherit Entity.entity
    val mutable hp : int = 10
    
   
    val mutable cursor : cursor_keys Js.t ref = cursors
    


    method! make : 'a. 'a -> int -> int -> unit =
      fun this newX newY ->
        x <- newX;
        y <- newY;
        movementPoints <- 1;
        tile <- 29;
        hp <- 10;
        moving <- false;
        let input : input_plugin t =
          (Js.Unsafe.eval_string {|x => x.input |} : _ -> _) this
        in
        console##log_2 (Js.string "input in player =  ") input;
        let cursor2 = input##.keyboard##createCursorKeys () in
        cursors := cursor2;
        cursor <- cursors;
        let add : game_object_creator t =
          (Js.Unsafe.eval_string {|x => x.add |} : _ -> _) this
        in
        let xx = !curr_map##tileToWorldX x in
        let yy = !curr_map##tileToWorldX y in
        sprite <- add##sprite xx yy (Js.string "tiles") tile;
        sprite##setOrigin 0.

    method! turnChar : twist t -> unit =
      fun twist ->
        let stuff =
          object%js
            val mutable moved : bool = false
            val mutable newX : int = x
            val mutable newY : int = y
          end
        in
        if movementPoints > 0 && moving == false then (
          if !cursor##.left##.isDown then (
            stuff##.newX := x - 1;
            stuff##.moved := true);
          if !cursor##.right##.isDown then (
            stuff##.newX := x + 1;
            stuff##.moved := true);
          if !cursor##.up##.isDown then (
            stuff##.newY := y - 1;
            stuff##.moved := true);
          if !cursor##.down##.isDown then (
            stuff##.newY := y + 1;
            stuff##.moved := true);
          if stuff##.moved == true then (
            movementPoints <- movementPoints - 1;
            if
              (Dungeon.dungeon self)##isWalkableTile stuff##.newX stuff##.newY
              == Optdef.return true
            then 
            (Dungeon.dungeon self)##moveEntityTo
              twist self stuff##.newX stuff##.newY));

        if hp <= 3 then
          sprite##.tint :=
            Js.Unsafe.eval_string {|Phaser.Display.Color.GetColor(255, 0, 0)|}
  end