open Js_of_ocaml
open Firebug
open Js
open Libgame.Phaser_bindings

let spr : sprite t ref = ref (Js.Unsafe.js_expr "1")

class type twist = object end

class entity =
  object 
    val mutable name : string = ""
    val mutable x : int = 0
    val mutable y : int = 0
    val mutable movementPoints : int = 1
    val mutable actionPoints : int = 0
    val mutable healthPoints : int = 0
    val mutable tile : int = 29
    val mutable moving : bool = false
    val mutable sprite : sprite t = !spr
    val mutable tweens : int = 0

    method refreshChar () =
      movementPoints <- 1;
      actionPoints <- 1

    method overChar () = movementPoints == 0 && moving == false
    method get_name = name 
    method get_x = x
    method set_x : int -> unit = fun xx -> x <- xx
    method get_y = y
    method set_y : int -> unit = fun yy -> y <- yy
    method get_tile = tile
    method get_moving = moving
    method set_moving : bool -> unit = fun mov -> moving <- mov
    method get_sprite = sprite
    method get_tweens = tweens
    method set_tweens : int -> unit = fun tw -> tweens <- tw
    method get_healthPoints = healthPoints
    method set_healthPoints : int -> unit = fun hp -> healthPoints <- hp
    method turnChar : twist t -> unit = fun twist -> ()
    method make : 'a. 'a -> int -> int -> unit = fun this newX newY -> ()
    method attack = 1
    method onDestroy () = console##log (Js.string "monster died")

  end
