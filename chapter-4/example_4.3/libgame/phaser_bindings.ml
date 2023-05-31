(** Copyright 2022, Winnie Pooh *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Change the name a make binding to phaser as separated library *)

open Js_of_ocaml 

open Js

class type spritesheetConfig =
  object
    method frameWidth : int readonly_prop
    method frameHeight : int readonly_prop
    method spacing : int readonly_prop
  end

class type twist = object end

class type config_map =
  object
    method data : int js_array t js_array t readonly_prop
    method tileWidth : int readonly_prop
    method tileHeight : int readonly_prop
  end

class type tileset = object end
class type dynamicLayer = object end

  
class type tile =
    object
      method index : int readonly_prop
    end

class type map =
  object
    method addTilesetImage :
      js_string t -> js_string t -> int -> int -> int -> int -> tileset t meth

    method createDynamicLayer :
      int -> tileset t -> int -> int -> dynamicLayer t meth

    method getTileAt : int -> int -> tile t meth
    method putTileAt : int -> int -> int -> unit meth
    method tileToWorldX : int -> int meth
    method tileToWorldY : int -> int meth
  end

class type game_object =
  object 
    method destroy : unit -> unit meth
    method active : bool prop
    method visible : bool prop
  end

class type sprite =
  object
  inherit game_object
    method x : int prop
    method y : int prop
    method tint : int prop
    method no : int prop
    method setOrigin : float -> unit meth
  end

class type game_object_creator =
  object
    method tilemap : config_map t -> map t meth
    method sprite : int -> int -> js_string t -> int -> sprite t meth
  end

class type loader_plugin =
  object
    method spritesheet :
      js_string t -> js_string t -> spritesheetConfig t -> unit meth
  end

class type tweensBuilderConfig =
  object
    method targets : sprite t readonly_prop
    method onComplete  :  (unit -> unit) readonly_prop
    method x : int readonly_prop
    method y : int readonly_prop
    method ease : js_string t readonly_prop
    method duration : int readonly_prop
    method hold : int readonly_prop
    method delay : int readonly_prop
    method yoyo : bool readonly_prop
  end

class type tween_manager =
  object
    method add : tweensBuilderConfig t -> unit meth
  end
  
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
  



