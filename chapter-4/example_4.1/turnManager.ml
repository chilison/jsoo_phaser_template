open Js_of_ocaml
open Firebug
open Js
open Player

module Entity = struct
  type t = Player.character
  type elt = Player.character

  let compare x y =
    let () = console##log (Js.string "compare") in
    compare x y
end

module SS = Set.Make (Entity)

let tm =
  object%js (this)
    val mutable currentIndex = 0
    val mutable entities = SS.empty

    method addEntity entity =
      this##.entities := SS.add entity this##.entities;
      console##log_2 (Js.string "entities = ") (SS.choose this##.entities)

    method removeEntity entity = SS.remove entity this##.entities

    method refresh () =
      SS.iter (fun x -> x#refreshChar ()) this##.entities;
      this##.currentIndex := 0

    method turn : twist Js.t -> unit =
      fun twist ->
        if SS.is_empty this##.entities <> true then
          SS.iter
            (fun x -> if x#overChar () == false then x#turnChar twist)
            this##.entities
        else this##.currentIndex := this##.currentIndex + 1

    method over () = SS.for_all (fun x -> x#overChar ()) this##.entities
  end
