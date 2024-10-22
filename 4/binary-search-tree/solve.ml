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

let count_comparisons (t : 'a btree) = 
  comparisons := 0;
  let result = is_bstree t compare in
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