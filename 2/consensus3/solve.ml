let consensus3 (f0, f1, f2) n =
  match (f0 n, f1 n, f2 n) with
  | a, b, _ when a = b -> Some a
  | a, _, c when a = c -> Some a
  | _, b, c when b = c -> Some b
  | _ -> None
