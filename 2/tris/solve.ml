let tris : 'a * 'a * 'a * 'a -> bool = function
  | a, b, c, _ when a = b && b = c -> true
  | a, b, _, c when a = b && b = c -> true
  | a, _, b, c when a = b && b = c -> true
  | _, a, b, c when a = b && b = c -> true
  | _ -> false

let my_rand () = Random.int 10 + 1
let hand () = (my_rand(), my_rand(), my_rand(), my_rand())

;;

assert (tris (1,1,1,42));;
assert (tris (1,1,2,42) |> not);;
assert (tris (1,3,2,42) |> not);;
assert (tris (1,3,2,42) |> not);;