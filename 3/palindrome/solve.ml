(* Two ways to do it: *)
let rev (l : 'a list) : 'a list =
  let rec go (accu : 'a list) : 'a list -> 'a list = function
  | [] -> accu
  | x :: xs -> go (x :: accu) xs
  in go [] l

let is_palindrome l = List.for_all2 (fun a b -> a = b) l (rev l)