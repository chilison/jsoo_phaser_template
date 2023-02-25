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
    val mutable interval : float = 15000.
    val mutable entities = SS.empty
    val mutable lastCall : float = Js.Unsafe.eval_string {|new Date ()|}

    method addEntity entity =
      this##.entities := SS.add entity this##.entities;
      console##log_2 (Js.string "entities = ") (SS.choose this##.entities)

    method removeEntity entity = SS.remove entity this##.entities
    method refresh () = SS.iter (fun x -> x#refreshChar ()) this##.entities

    method turn () =
      let now = Js.Unsafe.eval_string {|new Date ()|} in
      let limit : float =
        float_of_int
          (int_of_float this##.lastCall + int_of_float this##.interval)
      in
      if now > limit then (
        SS.iter
          (fun x -> if x#overChar () == false then x#turnChar ())
          this##.entities;

        this##.lastCall := Js.Unsafe.eval_string {|new Date ()|})

    method over () = SS.for_all (fun x -> x#overChar ()) this##.entities
  end
