let is_posfrac = function
  | _, 0 -> failwith "not a fraction"
  | a, b -> a * b > 0

let compare_frac (a, b) (c, d) =
  let mu = b * d in
  (* any common multiple works *)
  compare (a * (mu / b)) (c * (mu / d))

let compare_posfrac f1 f2 =
  match (f1, f2) with
  | _ when (not (is_posfrac f1)) || not (is_posfrac f2) ->
      failwith "not a posfrac"
  | _ -> compare_frac f1 f2
