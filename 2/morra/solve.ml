type winner = Player | Computer | Tie

let win (hp,gp) : (int * int) * winner =
  let hc,gc = Random.(int 5, int 10) in
  let winner =
    match gp = hp + hc, gc = hp + hc with
    | true, false -> Player
    | false, true -> Computer
    | _ -> Tie
  in ((hc,gc), winner)
