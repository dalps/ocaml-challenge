type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let rec forall (test : 'a -> bool) : 'a btree -> bool =
  function
  | Empty -> true
  | Node(v,left,right) ->
    forall test left &&
    forall test right &&
    test v

let comparisons : int ref = ref 0

let rec is_bstree (t : 'a btree) (comp : 'a -> 'a -> int) : bool =
  match t with
  | Empty -> true
  | Node(v,left,right) ->
    is_bstree left comp &&
    forall (fun n -> incr comparisons; comp n v < 0) left &&
    forall (fun n -> incr comparisons; comp v n < 0) right &&
    is_bstree right comp

let count_comparisons (test : 'a btree -> bool) (t : 'a btree) = 
  comparisons := 0;
  let result = test t in
  Printf.printf "Test result: %b\nComparisons: %d\n" result !comparisons

let t : int btree =
  Node(7,
    Node(4,
      Node(1,Empty,Empty),
      Node(5,Empty,Empty)),
    Node(10,Empty,Empty))
;;

assert (is_bstree t compare);;

let rec search (t : 'a btree) (comp : 'b -> 'a -> int) (x : 'b) : bool =
  match t with
  | Empty -> false
  | Node(v,left,right) ->
    match comp x v with
    | n when n < 0 -> search left comp x
    | n when n > 0 -> search right comp x
    | _ -> true
;;

assert (search t compare 1);;
assert (search t compare 7);;
assert (search t compare 10);;
assert (search t compare 42 = false);;

(* ### Slightly more efficient solution *)

let rec rightmost = function
  | Empty -> None
  | Node (v, _, Empty) -> Some v
  | Node (_, _, r) -> rightmost r

let rec leftmost = function
  | Empty -> None
  | Node (v, Empty, _) -> Some v
  | Node (_, l, _) -> leftmost l

let rec is_bstree_2 = function
  | Empty -> true
  | Node (v, l, r) ->
    let vl, vr = rightmost l, leftmost r in
    Option.fold ~none:true ~some:(fun vl -> incr comparisons; vl < v) vl &&
    Option.fold ~none:true ~some:(fun vr -> incr comparisons; v < vr) vr &&
    is_bstree_2 l && is_bstree_2 r

(* ### Extra stuff *)

(* Not a binary search tree... *)
let rec gen (accu : int btree) (step : int) (max_steps : int) : int btree =
  if step < max_steps then
    gen (match Random.int 3 with
    | 0 -> Empty
    | 1 -> Node (Random.int 10, accu, Empty)
    | _ -> Node (Random.int 10, Empty, accu)) (step + 1) max_steps
  else
    accu
    
let random_btree (max_steps : int) : int btree =
  gen Empty 0 max_steps

let rec insert (v : 'a) (compare : 'a -> 'a -> int) : 'a btree -> 'a btree = function
  | Empty -> Node (v, Empty, Empty)
  | Node (x, l, r) as t ->
    match compare v x with
    | n when n < 0 -> Node (x, insert v compare l, r)
    | n when n > 0 -> Node (x, l, insert v compare r)
    | _ -> t

let random_list () = List.init (Random.int 20) (fun _ -> Random.int 10)

let random_bstree () =
  random_list () |>
  List.fold_left (fun t v -> insert v compare t) Empty