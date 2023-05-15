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

let cursors : cursor_keys Js.t ref = ref (Js.Unsafe.js_expr "1")
let spr : sprite Js.t ref = ref (Js.Unsafe.js_expr "2")

class character =
  object (self : 'self)
    inherit Entity.entity
    
    val mutable cursor : cursor_keys Js.t ref = cursors

    method! make : 'a. 'a -> int -> int -> unit =
      fun this newX newY ->
        name <- "The Player";
        movementPoints <- 1;
        actionPoints <- 1;
        healthPoints <- 15;
        x <- newX;
        y <- newY;
        tile <- 29;
        moving <- false;
        let input : input_plugin t =
          (Js.Unsafe.eval_string {|x => x.input |} : _ -> _) this
        in 
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
                val mutable oldX : int = x
                val mutable oldY : int = y
                val mutable moved : bool = false
                val mutable newX : int = x
                val mutable newY : int = y
              end
            in
            if movementPoints > 0 then (
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
                     not((Dungeon.dungeon twist)##isWalkableTile stuff##.newX stuff##.newY)
                   then (
                     let enemy =
                       (Dungeon.dungeon twist)##entityAtTile stuff##.newX stuff##.newY
                     in
                     if enemy <> None && actionPoints > 0 then (
                       (Dungeon.dungeon twist)##attackEntity self (Option.get enemy);
                       actionPoints <- actionPoints - 1);
                     stuff##.newX := stuff##.oldX;
                     stuff##.newY := stuff##.oldY);
                   if stuff##.newX <> stuff##.oldX || stuff##.newY <> stuff##.oldY then (
                    (Dungeon.dungeon twist)##moveEntityTo
                       twist self stuff##.newX stuff##.newY;
                       )));

            if healthPoints <= 6 then 
              sprite##.tint :=
                Js.Unsafe.global##._Phaser##._Display##._Color##_GetColor 255 0 0;

          method! onDestroy = fun () -> 
            Dom_html.window##alert (Js.string "OMG! you died!");
            Dom_html.window##.location##reload
  end