type grade = Val of int | CumLaude

let is_valid : grade -> bool = function
  | CumLaude -> true
  | Val n when 18 <= n && n <= 30 -> true
  | _ -> false

let int_of_grade (g : grade) : int =
  if is_valid g then
    match g with
    | CumLaude -> 32
    | Val n -> n
  else
    failwith "Insufficient grade!"

let avg (grades : grade list) : float =
  let rec sum : float = List.fold_left (fun accu g -> (
    g |> int_of_grade |> Float.of_int) +. accu) 0.0 grades in
  let count = List.length grades |> Float.of_int in
  sum /. count