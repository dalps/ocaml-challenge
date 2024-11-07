let option_map f = function
  | None -> None
  | Some x -> Some (f x)

let double x = x * 2
let square x = x * x

;;

assert (option_map double (Some 3) = Some 6);;
assert (option_map double None = None);;
assert (option_map double (Some (square 3)) = Some 18);;
assert (option_map square (option_map double None) = None)