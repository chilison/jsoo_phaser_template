open Js_of_ocaml
open Firebug
open Js
open Dungeon

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

class type playerCharacter =
  object (self)
    val mutable x = 0
    val mutable y = 0
    val mutable movementPoints = 0
    val mutable sprite = 0

    method constructor x y =
      movementPoints = 1;
      console##log_2 (Js.string "input =  ") input;
      let cursor2 = input##.keyboard##createCursorKeys () in
      cursors := cursor2;
      x = x;
      y = y;
      sprite = 29;
      Dungeon.dungeon##map##putTileAt sprite x y

    method refresh () = movementPoints = 1

    method turn () =
      let oldX = x in
      let oldY = y in
      let moved = false in
      if movementPoints > 0 then (
        if cursors##.left##.isDown then x := x - 1 moved = true;
        if cursors##.right##.isDown then x := (x + 1) moved = true;
        if cursors##.up##.isDown then y := y + 1 moved = true;
        if cursors##.down##.isDown then y := y - 1 moved = true);
      let tileAtDestination = Dungeon.dungeon##.map##getTileAt (x, y) in
      if tileAtDestination##.index == Dungeon.dungeon##.sprites##.wall then
        x := oldX y := oldY;
      if x != oldX || y != oldY then
        Dungeon.dungeon##.map##putTileAt
          (sprite, x, y)
          Dungeon.dungeon##.map##putTileAt
          (Dungeon.dungeon##.sprites##.floor, oldX, oldY)

    method over () = movementPoints == 0
  end
