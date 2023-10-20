let tris (a, b, c, d) =
  (b = c && c = d) || (a = c && c = d) || (a = b && b = d) || (a = b && b = c)

let hand () =
  let rand () = Random.int 10 + 1 in
  (rand (), rand (), rand (), rand ())
;;

List.init 100 (fun _ ->
    let h = hand () in
    (h, tris h))
|> List.filter (fun (_, b) -> b = true)
