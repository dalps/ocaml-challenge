let dice p =
  match Random.int 100 with
  | x when x < p -> 6
  | _ -> Random.int 5 + 1
