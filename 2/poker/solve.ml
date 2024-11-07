type suit = S | H | D | C
type card = Card of int * suit
type hand = card * card * card * card * card

let rndHand () : hand = 
  let rndSuit () =
    match Random.int 4 with
    | 0 -> S
    | 1 -> H
    | 2 -> D
    | _ -> C
  in
  let rndRank () = Random.int 13 + 1 in
  let rndCard () = Card (rndRank (), rndSuit ()) in
  rndCard (), rndCard (), rndCard (), rndCard (), rndCard ()

let poker (c1, c2, c3, c4, c5) : bool =
  let test4 (Card (r1,s1)) (Card (r2,s2)) (Card (r3,s3)) (Card (r4,s4)) =
    r1 = r2 && r2 = r3 && r3 = r4 &&
    s1 <> s2 && s1 <> s3 && s1 <> s4 &&
    s2 <> s3 && s2 <> s4 &&
    s3 <> s4
  in
    test4 c1 c2 c3 c4 ||
    test4 c1 c2 c3 c5 ||
    test4 c1 c2 c4 c5 ||
    test4 c1 c3 c4 c5 ||
    test4 c2 c3 c4 c5

;;

assert (poker (Card (10, S), Card (10, S), Card (10, S), Card (10, S), Card (10, S)) = false);;
assert (poker (Card (10, S), Card (10, H), Card (10, D), Card (10, C), Card (1, C)));;