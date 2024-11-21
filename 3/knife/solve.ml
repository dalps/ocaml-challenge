let rec knife (l : 'a list) (n : int) : ('a list * 'a list) =
  match l with
  | [] -> ([], [])
  | _ when n <= 0 -> ([],l)
  | x :: xs ->
    let (left, right) = knife xs (n-1) in
    x :: left, right

;;

assert (knife [1;2;3;4;5;6] 3 = ([1;2;3], [4;5;6]));;
assert (knife ['b';'r';'e';'a';'d'] 3  = (['b';'r';'e'], ['a';'d']));;
assert (knife [] 3 = ([], []));;
assert (knife ["miss"; "me"] 2  = (["miss"; "me"], []));;
assert (knife ["oops"] (-1)  = ([], ["oops"]));;