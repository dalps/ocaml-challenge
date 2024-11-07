let consensus3 (f,g,h) (x : 'a) : 'b option =
  match f x, g x, h x with
  | fy, gy, _ when fy = gy -> Some fy
  | _, gy, hy when gy = hy -> Some gy
  | fy, _, hy when fy = hy -> Some hy
  | _ -> None

;;

assert (try 
  consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 0
    = Some 42 (* or whatever *)
  with _ -> true);;
assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 1 = Some 5);;
assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 2 = Some 2);;
assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 3 = None);;