let dice perc =
  match Random.int 100 with
  | p when p >= 100 - perc -> 6
  | _ -> Random.int 5 + 1

let dice2 perc =
  match Random.int 100 with p when p >= 100 - perc -> 6 | p -> (p mod 5) + 1
(* Avoids calling Random.int a second time but the values other
 * than 6 are not uniformly distributed when perc is not a multiple of 5.
 * e.g. for perc = 98 the PDF is: 
 * P(6) -> 0.98, P(5) -> 0, P(4) -> 0, P(3) -> 0, P(2) -> 0.1, P(1) -> 0.1 *)
