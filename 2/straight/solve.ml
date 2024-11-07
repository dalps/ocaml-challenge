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

let straight (Card (r1,_), Card (r2,_), Card (r3,_), Card (r4,_), Card (r5,_)) : bool =
  let smallest = min r1 @@ min r2 @@ min r3 @@ min r4 r5 in
  (* sm + sm+1 + sm+2 + sm+3 + sm+4 *)
  let scale = 5*smallest + 10 in
  scale - r1 - r2 - r3 - r4 - r5 = 0

;;

assert (straight (Card (10, S), Card (8, C), Card (6, C), Card (7, H), Card (9, H)));;
assert (straight (Card (1, H), Card (2, H), Card (3, C), Card (4, D), Card (5, C)));;
assert (straight (Card (10, S), Card (10, H), Card (10, D), Card (10, C), Card (1, C)) = false);;
