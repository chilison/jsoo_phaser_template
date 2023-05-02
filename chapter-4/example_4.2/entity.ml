open Js_of_ocaml
open Firebug
open Js

class type sprite =
object
  method tint : int prop
  method no : int prop
  method setOrigin : float -> unit meth
end

  let spr : sprite Js.t ref = ref (Js.Unsafe.js_expr "2")

class type twist = object end

class entity =
  object (self)
    val mutable x : int = 0
    val mutable y : int = 0
    val mutable movementPoints : int = 1
    val mutable tile : int = 29
    val mutable moving : bool = false
    val mutable sprite : sprite Js.t  = !spr
    method refreshChar () = movementPoints <- 1
    method overChar () = movementPoints == 0 && moving == false
    method get_x = x
    method set_x : int -> unit = fun xx -> x <- xx
    method get_y = y
    method set_y : int -> unit = fun yy -> y <- yy
    method get_tile = tile
    method get_moving = moving
    method set_moving : bool -> unit = fun mov -> moving <- mov
    method get_sprite = sprite
    method turnChar : twist t -> unit = fun twist -> ()
    method make : 'a. 'a -> int -> int -> unit =
      fun this newX newY -> ()
  end