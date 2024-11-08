let flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c =
  fun f b a -> f a b

let sub x y = x - y
let flipped_sub = flip sub;;

let ( -! ) = flipped_sub;;

assert (3 -! 10 = 7);;
assert (10 -! 3 = -7);;
