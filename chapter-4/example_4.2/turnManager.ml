open Js_of_ocaml
open Firebug
open Js
open Player

module Entity = struct
  type t = Entity.entity
  type elt = Entity.entity

  let compare x y =
    let () = console##log (Js.string "compare") in
    compare x y
end

module SS = Set.Make (Entity)

let tm =
  object%js (this)
    val mutable currentIndex : int = 0
    val mutable entities = SS.empty
    val mutable lastCall : float = Js.Unsafe.eval_string {|new Date ()|}
    method addEntity entity = this##.entities := SS.add entity this##.entities
    method removeEntity entity = SS.remove entity this##.entities

    method refresh () =
      SS.iter (fun x -> x#refresh ()) this##.entities;
      this##.currentIndex := 0

    method turn () =
      if SS.is_empty this##.entities <> true then
        SS.iter (fun x -> if x#over () == false then x#turn) this##.entities

    method over () = SS.for_all (fun x -> x#over ()) this##.entities
  end
