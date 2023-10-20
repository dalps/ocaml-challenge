type card = Joker | Val of int

let win p d =
  let is_valid c = c >= 1 && c <= 10 in
  match p,d with
  | Val p, Val d when is_valid p -> not (is_valid d) || p > d
  | _, Joker -> false
  | Joker, _ -> true
  | _ -> false