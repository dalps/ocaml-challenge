type suit = S | H | D | C
type card = Card of int * suit

let straight
    (Card (c1, _), Card (c2, _), Card (c3, _), Card (c4, _), Card (c5, _)) =
  let rec helper = function
    | n1 :: n2 :: t -> n2 = n1 + 1 && helper t
    | _ -> true
  in
  List.sort compare [ c1; c2; c3; c4; c5 ] |> helper
;;

assert (
  straight (Card (7, H), Card (4, D), Card (6, S), Card (5, H), Card (3, C)))
