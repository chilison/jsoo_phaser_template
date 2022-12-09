open Js_of_ocaml
open Firebug
open Js
(* open Base *)

class type entity = object end

module Entity = struct
  type t = entity

  let compare x y =
    let () = console##log (Js.string "compare =  ") in
    compare x y
end

module SS = Set.Make (Entity)

let tm =
  object%js
    val interval = 150
    val entities = SS.empty
    val lastCall = Unix.time ()
    method addEntity entity = entities.add entity
    method removeEntity entity = entities.remove entity
    method refresh entity = entities.empty

    method turn () =
      Firebug.console##log (Js.string "turn");
      console##log_2 (Js.string "self = ") this;
      let now = Unix.time () in
      let limit = tm##.lastCall + tm##.interval in
      if now > limit then ()
  end
