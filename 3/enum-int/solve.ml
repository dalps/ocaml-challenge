let rec enum_int n =
  match n with
  | 0 -> 0
  | n when n > 0 ->
      let n' = enum_int (n - 1) in
      if n' < 0 then -n' else -(n' + 1)
  | _ -> failwith "negative input"

(*
  0 -> 0
  1 -> 1
  2 -> -1
  3 -> 2
  4 -> -2
  5 -> 3
  6 -> -3
  7 -> 4
  8 -> -4
  9 -> 5
  10 -> -5
  ...
*)
