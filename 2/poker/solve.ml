type suit = S | H | D | C
type card = Card of int * suit

let rndCard () =
  let suit = match Random.int 4 with 0 -> S | 1 -> H | 2 -> D | _ -> C in
  Card (Random.int 13 + 1, suit)

let rndHand () = (rndCard (), rndCard (), rndCard (), rndCard (), rndCard ())

let fours_of hand =
  let rec helper n (a, b, c, d, e) =
    if n = 0 then [] else (a, b, c, d) :: helper (n - 1) (e, a, b, c, d)
  in
  helper 5 hand

let is_quads (Card (c1, s1), Card (c2, s2), Card (c3, s3), Card (c4, s4)) =
  List.sort compare [ s1; s2; s3; s4 ] = [ S; H; D; C ]
  && c1 = c2 && c2 = c3 && c3 = c4

let poker hand = List.exists is_quads (fours_of hand);;

assert (poker (Card (3, H), Card (3, D), Card (3, S), Card (8, H), Card (3, C)))

let test =
  List.init 10000 (fun _ ->
      let h = rndHand () in
      (h, poker h))
  |> List.filter (fun (_, b) -> b = true)
