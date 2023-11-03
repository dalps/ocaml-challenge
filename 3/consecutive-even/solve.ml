let consecutive_even l =
  List.fold_left
    (fun (c, m) a -> if a mod 2 = 0 then (1 + c, m) else (0, max c m))
    (0, 0) l
  |> snd
