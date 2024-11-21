let rec merge (l1 : int list) (l2 : int list) : int list =
  match l1, l2 with
  | [], _ -> l2
  | _, [] -> l1
  | h1 :: t1, h2 :: t2 ->
    if h1 < h2 then
      h1 :: merge t1 (h2 :: t2)
    else
      h2 :: merge (h1 :: t1) t2

let rec knife (l : 'a list) (n : int) : ('a list * 'a list) =
  match l with
  | [] -> ([], [])
  | _ when n <= 0 -> ([],l)
  | x :: xs ->
    let (left, right) = knife xs (n-1) in
    x :: left, right

let halve (l : 'a list) =
  knife l (List.length l / 2)

let rec merge_sort = function
  | [] -> []
  | [x] -> [x]
  | l ->
    let left, right = halve l in
    merge (merge_sort left) (merge_sort right)

;;
assert (merge [1;4;5] [2;3;6] = [1;2;3;4;5;6]);;
assert (merge [7] [2;3;6] = [2;3;6;7]);;
assert (merge [7] [] = [7]);;

assert (halve [1;3;5;8;-2;6] = ([1;3;5], [8;-2;6]));;
assert (halve [1;3] = ([1], [3]));;
assert (halve [1;3;5] = ([1], [3;5]));;

assert (merge_sort [1;3;5;8;-2;6] = [-2;1;3;5;6;8]);;