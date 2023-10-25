let string_of_list l =
  let rec helper = function
    | [] -> ""
    | [ n ] -> string_of_int n
    | n :: t -> string_of_int n ^ ";" ^ helper t
  in
  "[" ^ helper l ^ "]"
