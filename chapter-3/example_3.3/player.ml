open Js_of_ocaml
open Firebug
open Js
open Dungeon
open TurnManager

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
let curr_character ref = ref (Js.Unsafe.js_expr "1")

let make_player self xCoord yCoord =
  let input : input_plugin t =
    (Js.Unsafe.eval_string {|x => x.input |} : _ -> _) self
  in
  console##log_2 (Js.string "input in player =  ") input;
  let cursor2 = input##.keyboard##createCursorKeys () in
  cursors := cursor2;
  let character =
    object%js (self)
      val mutable x : int = xCoord
      val mutable y : int = yCoord
      val mutable movementPoints : int = 0
      val mutable sprite : int = 29
      method refresh () = self##.movementPoints = 1
      method over () = self##.movementPoints == 0

      (* method sprite = sprite *)
      (* method y = y *)
      (* method x = x *)

      method turn () =
        let stuff =
          object%js
            val mutable moved : bool = false
            val mutable oldX : int = self##.x
            val mutable oldY : int = self##.y
          end
        in
        if self##.movementPoints > 0 then (
          if !cursors##.left##.isDown then (
            self##.x := self##.x - 1;
            stuff##.moved := true);
          if !cursors##.right##.isDown then (
            self##.x := self##.x + 1;
            stuff##.moved := true);
          if !cursors##.up##.isDown then (
            self##.y := self##.y + 1;
            stuff##.moved := true);
          if !cursors##.down##.isDown then (
            self##.y := self##.y - 1;
            stuff##.moved := true));
        let tileAtDestination =
          Dungeon.(!curr_map)##getTileAt self##.x self##.y
        in
        if tileAtDestination == (Dungeon.dungeon self)##.sprites##.wall then (
          self##.x := stuff##.oldX;
          self##.y := stuff##.oldY);
        if self##.x != stuff##.oldX || self##.y != stuff##.oldY then
          Dungeon.(!curr_map)##putTileAt self##.sprite self##.x self##.y;
        Dungeon.(!curr_map)##putTileAt
          (Dungeon.dungeon self)##.sprites##.floor
          self##.x self##.y
    end
  in

  Dungeon.(!curr_map)##putTileAt character##.sprite character##.x character##.y;
  character