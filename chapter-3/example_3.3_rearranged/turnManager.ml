open Js_of_ocaml
open Firebug
open Js

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

class character =
  object (self)
    val mutable x : int = 0
    val mutable y : int = 0
    val mutable movementPoints : int = 0
    val mutable sprite : int = 29
    method refresh () = movementPoints <- 1
    method overChar () = movementPoints == 0

    method set_player newX newY =
      x <- newX;
      y <- newY

    method make_player this =
      let input : input_plugin t =
        (Js.Unsafe.eval_string {|x => x.input |} : _ -> _) this
      in
      console##log_2 (Js.string "input in player =  ") input;
      let cursor2 = input##.keyboard##createCursorKeys () in
      cursors := cursor2;
      Dungeon.(!curr_map)##putTileAt sprite x y

    method turn () =
      let stuff =
        object%js
          val mutable moved : bool = false
          val mutable oldX : int = x
          val mutable oldY : int = y
        end
      in
      if movementPoints > 0 then (
        if !cursors##.left##.isDown then (
          x <- x - 1;
          stuff##.moved := true);
        if !cursors##.right##.isDown then (
          x <- x + 1;
          stuff##.moved := true);
        if !cursors##.up##.isDown then (
          y <- y + 1;
          stuff##.moved := true);
        if !cursors##.down##.isDown then (
          y <- y - 1;
          stuff##.moved := true));
      let tileAtDestination = Dungeon.(!curr_map)##getTileAt x y in
      if tileAtDestination == (Dungeon.dungeon self)##.sprites##.wall then (
        x <- stuff##.oldX;
        y <- stuff##.oldY);
      if x != stuff##.oldX || y != stuff##.oldY then
        Dungeon.(!curr_map)##putTileAt sprite x y;
      Dungeon.(!curr_map)##putTileAt
        (Dungeon.dungeon self)##.sprites##.floor
        x y
  end

module Entity = struct
  type t = character
  type elt = character

  let compare x y =
    let () = console##log (Js.string "compare =  ") in
    compare x y
end

module SS = Set.Make (Entity)

let tm =
  object%js (this)
    val mutable interval : float = 150.
    val mutable entities = SS.empty
    val mutable lastCall : float = Js.Unsafe.eval_string {|new Date ()|}
    method addEntity entity = this##.entities := SS.add entity this##.entities
    method removeEntity entity = SS.remove entity this##.entities

    method refresh () =
      this##.entities :=
        SS.filter (fun x -> SS.mem x this##.entities == false) this##.entities

    method turn () =
      Firebug.console##log (Js.string "turn");
      console##log_2 (Js.string "self = ") this;
      let now = Js.Unsafe.eval_string {|new Date ()|} in
      let limit : float =
        float_of_int
          (int_of_float this##.lastCall + int_of_float this##.interval)
      in
      if now > limit then (
        SS.iter (fun x -> if x#overChar () then x#turn ()) this##.entities;
        this##.lastCall := Js.Unsafe.eval_string {|new Date ()|})

    method over () = SS.for_all (fun x -> x#overChar ()) this##.entities
  end
