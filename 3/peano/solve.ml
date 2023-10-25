type nat = Z | S of nat

let rec nat_of_int = function
  | 0 -> Z
  | n when n >= 0 -> S (nat_of_int (n - 1))
  | _ -> failwith "negative input"

let rec int_of_nat = function Z -> 0 | S n -> 1 + int_of_nat n
let rec is_even = function Z -> true | S Z -> false | S (S n) -> is_even n
let rec halve = function Z | S Z -> Z | S (S n) -> S (halve n)
let rec sum a b = match (a, b) with a, Z -> a | a, S b -> S (sum a b)
let rec mul a b = match (a, b) with _, Z -> Z | a, S b -> sum a (mul a b)

let rec equals a b =
  match (a, b) with Z, Z -> true | S a, S b -> equals a b | _ -> false

let rec leq a b =
  match (a, b) with Z, _ -> true | S a, S b -> leq a b | _ -> false
