let adds_to (goal : int) (l : int list) : int list list =
  let rec helper (diff : int) : int list -> int list list =
    function
    | [] ->
      if diff = 0 then
        [[]] (* Good to go *)
      else
        [] (* Couldn't reach goal, don't start a new combo *)
    | x :: xs ->
      if x > diff then
        (* Ignore first element if too big to contribute to any combo *)
        helper diff xs
      else
        helper diff xs @
        (* Intuition: the combinations that sum up to [n-x] in [xs]
           naturally sum up to [n] when you add [x] to them *)
        List.map (List.cons x) (helper (diff-x) xs)
  in helper goal l
;;

assert (adds_to 3 [1;2;3] = [[3]; [1;2]]);;
assert (adds_to 3 [0;1;2;3] = [[3]; [1;2]; [0;3]; [0;1;2]]);;
assert (adds_to 15 [1;3;5] = []);;
assert (adds_to 15 [1;2;3;4;5] = [[1;2;3;4;5]]);;
assert (adds_to 15 [1;2;3;4;5;10] = [[5;10]; [2;3;10]; [1;4;10]; [1;2;3;4;5]]);;

