open Js_of_ocaml
open Firebug
open Js

[@@@ocamlformat "disable"]

let room1 = [|
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|];
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554|];
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|];
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554|];
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554|];
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554|];    
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554|];
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554|];
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|];
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]; 
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|];
   [|554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554; 554|]
|]
|> Array.map Js.array

let level2 =  Js.array room1
