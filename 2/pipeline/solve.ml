let ( |> ) x f = f x 

let double x = x * 2
let square x = x * x

;;

assert (3 |> double = double 3);;
assert (3 |> double |> square = square (double 3));;
assert (3 |> double |> square |> double = double (square (double 3)));;
