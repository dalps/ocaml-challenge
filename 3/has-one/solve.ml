let has_one x : bool =
  assert (x >= 0);
  let rec seek_one n =
    if n = 0 then false else
    let lsd, rest = n mod 10, n/10 in
    lsd = 1 || seek_one rest
  in seek_one x

;;

assert(has_one 10 = true);;
assert(has_one 220 = false);;
assert(has_one 911 = true);;
assert(has_one 451 = true);;
assert(try has_one (-1) |> fun _ -> false with _ -> true);;