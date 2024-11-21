(* Two ways to do it: *)

let string_of_list (l : int list) =
  let rec helper = function
  | [] -> ""
  | [x] -> string_of_int x
  | x :: xs -> string_of_int x ^ ";" ^ helper xs
  in
  "[" ^ helper l ^ "]"

let string_of_list_2 (l : int list) : string =
  match l with
  | [] -> "[]"
  | x :: xs -> "[" ^ string_of_int x ^
    let rec helper = function
    | [] -> ""
    | y :: ys -> ";" ^ string_of_int y ^ helper ys
    in helper xs ^ "]"

