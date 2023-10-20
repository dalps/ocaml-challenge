let seven_eleven_doubles () =
  let throw () = (Random.int 6) + 1 in
  let d1, d2 = throw (), throw () in
  (
    d1 = d2 || d1 + d2 = 7 || d1 + d2 = 11,
    d1,
    d2
  )