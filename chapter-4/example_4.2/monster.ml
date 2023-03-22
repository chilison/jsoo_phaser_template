open Js_of_ocaml
open Js
open Firebug
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

class basicMonster =
  object (self)
    inherit entity
    method! get_sprite : int = !sprite##.no

    method! make : 'a. 'a -> unit =
      fun _this -> Dungeon.(!curr_map)##putTileAt tile x y

    method! turn =
      let stuff1 =
        object%js
          val mutable oldX : int = x
          val mutable oldY : int = y
        end
      in
      if movementPoints > 0 then (
        let stuff2 =
          object%js
            val mutable pX : int = 42
            val mutable pY : int = 42
          end
        in
        let stuff3 =
          object%js
            (* я не уверена, если здесь действительно можно передавать просто изначальный массив, в оригинале левел из данжона *)
            val mutable grid =
              Js.Unsafe.eval_string {|new PF.Grid(dungeon.level)|}

            val mutable finder = Js.Unsafe.eval_string {|new PF.AStarFinder()|}
          end
        in
        let stuff4 =
          object%js
            val mutable path =
              (stuff3##.finder)#findPath stuff1##.oldX stuff1##.oldY stuff2##.pX
                stuff2##.pY stuff3##.grid
          end
        in
        if (stuff4##.path)#length > 2 then (
          (Dungeon.dungeon self)##moveEntityTo
            (object%js
               val mutable x : int = self#get_x
               val mutable y : int = self#get_y
               val mutable moving : bool = self#get_moving
               val mutable sprite : int = self#get_sprite
            end)
            stuff4##.path [ 1 ] [ 0 ] stuff4##.path [ 1 ] [ 1 ];
          movementPoints <- movementPoints - 1);
        if stuff2##.pX == 42 && stuff1##.oldX == 42 then
          movementPoints <- movementPoints - 1)
  end