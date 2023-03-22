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
let spr : sprite Js.t ref = ref (Js.Unsafe.js_expr "1")

class character =
object (self: 'self)
    val mutable x : int = 0
    val mutable y : int = 0
    val mutable movementPoints : int = 1
    val mutable tile : int = 29
    val mutable hp : int = 10
    val mutable moving : bool = false
    val mutable sprite : sprite Js.t = !spr
    val mutable cursor : cursor_keys Js.t ref = cursors
    method refreshChar () = movementPoints <- 1
    method get_x = x
    method get_y = y
    method get_moving = moving
    method get_sprite : int = sprite##.no
    method overChar () = movementPoints == 0

    method set_player newX newY =
      x <- newX;
      y <- newY

    method make_player : 'a. 'a -> unit =
      fun this ->
        let input : input_plugin t =
          (Js.Unsafe.eval_string {|x => x.input |} : _ -> _) this
        in
        console##log_2 (Js.string "input in player =  ") input;
        let cursor2 = input##.keyboard##createCursorKeys () in
        cursors := cursor2;
        cursor <- cursors;
        Dungeon.(!curr_map)##putTileAt tile x y

    method turnChar  =
      let stuff =
        object%js
          val mutable moved : bool = false
          val mutable newX : int = x
          val mutable newY : int = y
        end
      in
      if movementPoints > 0 && moving == false then (
        
        if !cursor##.left##.isDown then (
          console##log (Js.string "1");
          stuff##.newX := x - 1;
          stuff##.moved := true);
        if !cursor##.right##.isDown then (
          console##log (Js.string "2");
          stuff##.newX := x + 1;
          stuff##.moved := true);
        if !cursor##.up##.isDown then (
          console##log (Js.string "3");
          stuff##.newY := y - 1;
          stuff##.moved := true);
        if !cursor##.down##.isDown then (
          console##log (Js.string "4");
          stuff##.newY := y + 1;
          stuff##.moved := true);
        if stuff##.moved == true then (
          movementPoints <- movementPoints - 1;
          if (Dungeon.dungeon self)##isWalkableTile stuff##.newX stuff##.newY == Optdef.return (true)
          then
         
          (Dungeon.dungeon self)##moveEntityTo 
          (object%js
          val mutable x : int = self#get_x
          val mutable y : int = self#get_y
          val mutable moving : bool = self#get_moving
          val mutable sprite : int = self#get_sprite
            end) stuff##.newX stuff##.newY;
            stuff##.moved := true)

        );

      if hp <= 3 then
        sprite##.tint := 
        Js.Unsafe.eval_string {|Phaser.Display.Color.GetColor(255, 0, 0)|} 
  end