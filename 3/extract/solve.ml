let[@tail_mod_cons] rec ( @ ) l1 l2 =
  match l1 with [] -> l2 | h :: t -> h :: (t @ l2)
(* @ can be made tail-recursive using List.rev, but that transformation would
   make it slower. It can be optimized by the compiler instead, since it
   recursive call appears in tail-modulo-cons position.
   (note that Stdlib.( @ ) is not tail-recursive) *)

let extract i l =
  let rec helper carry n l =
    match l with
    | [] -> failwith "nth"
    | h :: t when n = i -> (h, carry @ t)
    | h :: t -> helper (carry @ [ h ]) (n + 1) t
  in
  helper [] 0 l
;;

assert (extract 3 [ 1; 2; 4; 2; 3; 2; 0 ] = (2, [ 1; 2; 4; 3; 2; 0 ]))
