let rec enum_nat_nat n =
  if n = 0 then (0, 0)
  else
    match enum_nat_nat (n - 1) with a, 0 -> (0, a + 1) | a, b -> (a + 1, b - 1)
