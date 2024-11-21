let movie_rating a b c =
  let check_out_of_range n = n < 1 || n > 5 in
  match a, b, c with
  | a, b, c when
    check_out_of_range a ||
    check_out_of_range b ||
    check_out_of_range c ->
    failwith "Out of range"
  | 5, 5, 5 -> "Masterpiece"
  | 5, 5, c when c >= 4 -> "Highly Recommended"
  | 5, c, 5 when c >= 4 -> "Highly Recommended"
  | c, 5, 5 when c >= 4 -> "Highly Recommended"
  | a, b, c when a >= 4 && b >= 4 && c >= 3 -> "Recommended"
  | a, b, c when a >= 4 && c >= 4 && b >= 3 -> "Recommended"
  | a, b, c when b >= 4 && c >= 4 && a >= 3 -> "Recommended"
  | _ -> "Mixed Reviews"
