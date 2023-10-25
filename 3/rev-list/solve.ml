let rec rev = function [] -> [] | h :: t -> rev t @ [ h ]

let rev_tr l =
  let rec helper l m =
    match (l, m) with [], l -> l | h :: t, l -> (helper [@tailcall]) t (h :: l)
  in

  helper l []

(* rev_tr is indeed tail recursive: it is transformed to a loop in the object 
 * code. Compare implementations of rev and rev_tr in the output of
 * ocamlopt -c -S solve.ml *)
