let first_third_fifth : 'a list -> ('a * 'a * 'a)option = function
  | [] |  [_] | [_; _] | [_; _; _] | [_; _; _; _] -> None
  | x0 :: _ :: x2 :: _ :: x4 :: _ -> Some (x0, x2, x4) 