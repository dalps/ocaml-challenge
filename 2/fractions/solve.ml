let is_posfrac (a, b) : bool = b <> 0 && a * b > 0

let compare_posfrac (a1, b1) (a2, b2) : int =
  assert (is_posfrac (a1, b1));
  assert (is_posfrac (a2, b2));
  compare (a1 * b2) (a2 * b1)

let compare_frac ((a1, b1) as f1) ((a2, b2) as f2) : int =
  if is_posfrac f1 then
    if is_posfrac f2 then
      compare_posfrac f1 f2
    else 1
  else
    if is_posfrac f2 then -1
    else compare (a2 * b1) (a1 * b2)
;;

assert (compare_posfrac (1,2) (2,4) == 0);;
assert (compare_posfrac (1,2) (1,3) == 1);;
assert (compare_posfrac (1,2) (2,3) == -1);;

assert (compare_frac (-1,2) (1,3) == -1);;
assert (compare_frac (-1,2) (2,3) == -1);;
assert (compare_frac (1,2) (-2,3) == 1);;
assert (compare_frac (1,2) (2,-3) == 1);;
assert (compare_frac (-1,-2) (2,-3) == 1);;
assert (compare_frac (-1,2) (2,-3) == 1);;
assert (compare_frac (2,2) (4,4) == 0);;
assert (compare_frac (-1,-18) (2,-36) == 1);;
assert (compare_frac (-1,-18) (2,36) == 0);;
assert (compare_frac (1,-18) (-2,36) == 0);;