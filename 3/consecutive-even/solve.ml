let consecutive_even l =
  List.fold_left
    (fun (c, m) a ->
      let c' = if a mod 2 = 0 then 1 + c else 0 in
      let m' = max c' m in
      (c', m'))
    (0, 0) l
  |> snd
