open Js_of_ocaml
open Firebug
open Js
open Entity

module Entity = struct
  type t = Entity.entity
  type elt = Entity.entity

  let compare x y =
    compare x y
end

module SS = Set.Make (Entity)

let tm =
  object%js (this)
    val mutable currentIndex : int = 0
    val mutable entities = SS.empty
    val mutable lastCall : float = Js.Unsafe.eval_string {|new Date ()|}
    method addEntity entity = this##.entities := SS.add entity this##.entities
    method removeEntity : entity -> unit =  fun entity -> this##.entities := SS.remove entity this##.entities

    method refresh () =
      SS.iter (fun x -> x#refreshChar ()) this##.entities;
      this##.currentIndex := 0

    method turn : twist Js.t -> unit =
      fun twist ->
        if SS.is_empty this##.entities <> true then
          SS.iter
            (fun x -> if not (x#overChar () ) then x#turnChar twist)
            this##.entities
        else 
           this##.currentIndex := this##.currentIndex + 1

    method over () = SS.for_all (fun x -> x#overChar ()) this##.entities
  end
