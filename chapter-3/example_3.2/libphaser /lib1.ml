(** Copyright 2022, Winnie Pooh *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let fib = function
  | 0 -> 0
  | 1 -> 1
  | n ->
      let rec helper prev x n =
        if n <= 0 then x else helper x (prev + x) (n - 1)
      in
      helper 0 1 n

let%test _ = fib 4 = 5
