let rec gcd a b =
  if b = 0 then a
  else
    let r = a mod b in
    gcd b r
