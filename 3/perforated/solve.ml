let rec is_perforated : int list -> bool = function
  | [] -> true
  | [_] -> true
  | m :: n :: ns ->
    Int.abs (m - n) > 1 && is_perforated (n :: ns)
;;

assert(is_perforated []);;
assert(is_perforated [1]);;
assert(is_perforated [1;2] = false);;
assert(is_perforated [1;3]);;
assert(is_perforated [1;5;2]);;
assert(is_perforated [1;3;2] = false);;
assert(is_perforated [1;4;2;0]);;
assert(is_perforated [1;3;2;0] = false);;
assert(is_perforated [1;3;5;2;4;7;3;1]);