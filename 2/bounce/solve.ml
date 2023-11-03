let bounce t x =
  match x / t with y when y mod 2 = 0 -> x mod t | _ -> t - (x mod t)
