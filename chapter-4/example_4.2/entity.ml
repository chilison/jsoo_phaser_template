open Js_of_ocaml
open Firebug
open Js

class type sprite =
  object
    method tint : int prop
    method no : int prop
  end

let spr : sprite Js.t ref = ref (Js.Unsafe.js_expr "1")

class entity =
  object (self)
    val mutable x : int = 0
    val mutable y : int = 0
    val mutable movementPoints : int = 1
    val mutable tile : int = 29
    val mutable moving : bool = false
    val mutable sprite : sprite Js.t ref = spr
    method refresh () = movementPoints <- 1
    method over () = movementPoints == 0 && moving == false
    method get_x = x
    method get_y = y
    method get_tile = tile
    method get_moving = moving
    method get_sprite = 10
    method turn = ()
    method make : 'a. 'a -> unit = fun this -> console##log (Js.string "hoo;f")

    method set newX newY =
      x <- newX;
      y <- newY
  end