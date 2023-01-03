open Js_of_ocaml
open Firebug
open Js

class type entity =
  object
    method x : int prop
    method y : int prop
    method sprite : int prop
    method movementPoints : int prop
    method over : unit -> bool meth
    method turn : unit -> unit meth
    method refresh : unit -> unit meth
  end

module Entity = struct
  type t = entity Js.t

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
    method addEntity entity = SS.add entity this##.entities
    method removeEntity entity = SS.remove entity this##.entities
    method refresh = this##.entities = SS.empty

    method turn () =
      Firebug.console##log (Js.string "turn");
      console##log_2 (Js.string "self = ") this;
      let now = Js.Unsafe.eval_string {|new Date ()|} in
      let limit : float =
        float_of_int
          (int_of_float this##.lastCall + int_of_float this##.interval)
      in
      if now > limit then (
        SS.iter
          (fun x -> if x##over () == false then x##turn ())
          this##.entities;
        this##.lastCall := Js.Unsafe.eval_string {|new Date ()|})

    method over () = SS.for_all (fun x -> x##over ()) this##.entities
  end
