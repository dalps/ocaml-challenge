let count c = List.fold_left (fun acc c' -> if c = c' then 1 + acc else acc) 0;;

assert (count 0 [ 0; 0; 2; 1; 0 ] = 3);;
assert (count 0 [ 2; 1 ] = 0)

let rec lang1 = function
  | [] -> true
  | 0 :: t -> count 0 t = 0
  | 1 :: t -> lang1 t
  | _ -> false
;;

assert (lang1 [ 1; 1; 1; 1; 0 ]);;
assert (lang1 [ 0; 1; 1; 0 ] = false);;
assert (lang1 [ 0; 1; 1; 1 ]);;
assert (lang1 [ 1; 0; 1; 1; 1 ])

let rec lang2 = function
  | [] -> true
  | 1 :: t -> count 0 t = 0
  | 0 :: t | 2 :: t -> lang2 t
  | _ -> false
;;

assert (lang2 [ 0; 2; 1; 2; 1; 1 ])

let rec lang3 = function
  | [] -> true
  | 0 :: t -> ( match t with 1 :: 1 :: _ -> true | _ -> false)
  | _ :: t -> lang3 t
;;

assert (lang3 [ 3; 0; 1; 1; 4 ])

let lang4 w = count 0 w >= count 1 w
let lang5 w = count 0 w = count 1 w

let lang6 w =
  let rec helper n st l =
    match (st, l) with
    | _, [] -> n = 0
    | `Zero, 0 :: t -> helper (n + 1) `Zero t
    | _, 1 :: t -> helper (n - 1) `One t
    | _ -> false
  in
  helper 0 `Zero w
;;

assert (lang6 [ 0; 0; 0; 1; 1; 1 ]);;
assert (lang6 [ 0; 0; 0; 1; 1; 0 ] = false)

let lang7 w =
  let rec helper n op = function
    | [] -> n = 0
    | 0 :: t -> helper (op n 1) op t
    | 1 :: t -> helper n ( - ) t
    | _ -> false
  in
  helper 0 ( + ) w
;;

assert (lang7 [ 0; 0; 0; 1; 0; 0; 0 ])

let rec lang8 = function
  | [] -> true
  | 1 :: t -> count 2 t = count 0 t
  | 0 :: t | 2 :: t -> lang8 t
  | _ -> false
;;

assert (lang8 [ 2; 0; 1; 2; 0; 2; 0 ])

let lang9 w =
  let rec helper n = function
    | [] -> n = 0
    | 0 :: t -> helper (n + 1) t
    | c :: t when c <> 1 -> helper n t
    | 1 :: t -> count 1 t = n - 1 && count 0 t = 0
    | _ -> false
  in
  helper 0 w
;;

assert (lang9 [ 2; 0; 3; 2; 0; 2; 0; 1; 6; 1; 1; 5; 9 ]);;
assert (lang9 [ 2; 0; 3; 2; 0; 2; 1; 6; 1; 1; 5; 9 ] = false);;
assert (lang9 [ 2; 0; 3; 2; 0; 2; 0; 1; 0; 6; 1; 1; 5; 9 ] = false);;
assert (lang9 [ 2; 0; 3; 2; 2; 0; 1; 0; 6; 1; 1; 5; 9 ] = false);;
