let both_true (p : 'a -> bool) (q : 'a -> bool) : 'a -> bool =
  fun x -> p x && q x

let is_positive x = x > 0
let is_even x = x mod 2 = 0
let is_positive_and_even = both_true is_positive is_even;;

assert(is_positive_and_even 4 = true);;
assert(is_positive_and_even (-2) = false);;
assert(is_positive_and_even 3 = false);;
assert(is_positive_and_even (-3) = false);;