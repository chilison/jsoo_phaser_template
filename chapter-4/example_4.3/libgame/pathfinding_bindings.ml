open Js_of_ocaml 

open Js

type grid

class type path =
  object
    method length : int prop
    
  end

class type finder =
  object
    method findPath : int -> int -> int -> int -> grid t -> path t meth
  end
