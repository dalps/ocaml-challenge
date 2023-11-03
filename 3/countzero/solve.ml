let rec countzero f a b =
  if a > b then 0 else (if f a = 0 then 1 else 0) + countzero f (a + 1) b
