let rec rnd_list n b =
  if n <= 0 then
    []
  else
    (Random.int b + 1) :: rnd_list (n-1) b