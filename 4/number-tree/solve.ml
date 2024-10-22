type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let number_tree (t : 'a btree) : (int * 'a) btree =
  let rec go (step : int) : 'a btree -> int * (int * 'a) btree =
    function
    | Empty -> (step, Empty)
    | Node(v,left,right) ->
      let step_left, numbered_left = go step left in
      let step_right, numbered_right = go (step_left + 1) right in
      (step_right, Node ((step_left, v), numbered_left, numbered_right))
    in
  snd (go 0 t)

let t =
  Node("d",
    Node("c",
      Node("a",
        Empty,
        Node("b",Empty,Empty)),
      Empty),
    Node("e",Empty,Empty))
;;

assert (
  number_tree t =
  Node((3,"d"),
    Node((2,"c"),
      Node((0,"a"),
        Empty,
        Node((1,"b"),Empty,Empty)),
    Empty),
  Node((4,"e"),Empty,Empty)))