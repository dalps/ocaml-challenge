let is_even n = n mod 2 = 0

let win a b =
  let is_valid n = n >= 1 && n <= 5 in
  match a,b with
  | _ when is_valid a && not (is_valid b) -> 1
  | _ when is_valid b && not (is_valid a) -> -1
  | _ when is_valid a && is_valid b -> if a + b |> is_even then 1 else -1
  | _ -> 0