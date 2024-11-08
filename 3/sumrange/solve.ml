let sumrange a b =
  let sumrange0 a = if a > 0 then a * (a+1) / 2 else 0 in
  if b-a < 0 then
    0
  else
    (* a + a+1 + a+2 + a+3 + ... + a+(b-a) *)
    a*(b-a+1) + sumrange0 (b-a)
;;

assert (sumrange 0 1 = 1);;
assert (sumrange 1 3 = 6);;
assert (sumrange 3 2 = 0);;
assert (sumrange 7 15 = 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15);;