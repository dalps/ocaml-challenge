type winner = Player | Computer | Tie

let win (hp, gp) =
  let hc = Random.int 6 in
  let gc = Random.int 6 + hc in
  (* Guessing below hc or above hc+5 would be silly:
   * gc = rand(max - min + 1) + min 
   *    = rand(hc + 5 - hc + 1) + hc
   *    = rand(6) + hc 
   *)
  let winner =
    match hp + hc with
    | n when n = gp && gp <> gc -> Player
    | n when n = gc && gp <> gc -> Computer
    | _ -> Tie
  in
  ((hc, gc), winner)
