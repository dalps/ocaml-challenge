let rec digits_of n = if n > 0 then (n mod 10) :: digits_of (n / 10) else []

let rec alt_even n =
  digits_of n
  |> List.filteri (fun i d -> if i mod 2 = 0 then d mod 2 = 1 else d mod 2 = 0)
  |> ( = ) []
