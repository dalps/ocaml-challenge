let consecutive_even (l : int list) : int =
  let rec go (accu, res) = function
  | [] -> res
  | x :: xs ->
    let accu' = if x mod 2 = 0 then 1 + accu else 0 in
    go (accu', max accu' res) xs
  in go (0, 0) l
;;

assert(consecutive_even [] = 0);;
assert(consecutive_even [1;2;3;4;5;6] = 1);; 
assert(consecutive_even [1;2;2;3;4;5] = 2);;
assert(consecutive_even [1;2;3;4;2;5] = 2);;
assert(consecutive_even [1;2;2;3;4;2;5] = 2);;
assert(consecutive_even [1;2;2;2;3;4;2;6;5] = 3);;
assert(consecutive_even [1;2;2;2;3;4;2;6;8;42;5;0;0] = 5);;