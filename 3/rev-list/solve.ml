let rev (l : 'a list) : 'a list =
  let rec go : 'a list -> 'a list = function
  | [] -> []
  | x :: xs -> go xs @ [x]
  in go l

let rev_tr (l : 'a list) : 'a list =
  let rec go (accu : 'a list) : 'a list -> 'a list = function
  | [] -> accu
  | x :: xs -> go (x :: accu) xs
  in go [] l
