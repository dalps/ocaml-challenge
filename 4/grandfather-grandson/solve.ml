let exchange x =
  if x >= 100 then failwith "Must be < 100";
  let d0, d1 = x mod 10, x / 10 in
  d0 * 10 + d1

let is_valid_answer (granpa_age, grandson_age) =
  granpa_age = 4 * grandson_age &&
  exchange grandson_age = 3 * exchange granpa_age

(** [gen_pairs a b] generates pairs where the first value
    is strictly bigger than the value and both values are in the range from [a] (included) to [b] (excluded)
    
    [ gen_pairs 0 4 = [
      (0, 0); (1, 0); (2, 0); (3, 0);
      (1, 1); (2, 1); (3, 1);
      (2, 2); (3, 2);
      (3, 3)
    ] ]*)
let gen_pairs (start : int) (finish : int) =
  let rec go (i : int) =
    if i <= finish then
      List.init (finish-i) (fun x -> (x+i,i)) @ go (i+1)
    else
      []
  in go start
;;

assert (gen_pairs 0 99 |> List.filter is_valid_answer =  [(0, 0); (72, 18)]);;

let find (max_grandpa_age, min_grandson_age) : (int * int) =
  gen_pairs min_grandson_age max_grandpa_age |>
  List.filter is_valid_answer |>
  List.find (fun (a,b) -> a <> 0 || b <> 0)
;;

assert (find (99,0) = (72, 18))

