let rec minmaxfun f a b =
  match b - a with
  | n when n < 0 -> None
  | 0 -> Some (f a, f a)
  | _ -> (
      match minmaxfun f (a + 1) b with
      | Some (mn, mx) -> Some (min (f a) mn, max (f a) mx)
      | None -> Some (f a, f a))

let f x =
  let x = float_of_int x in
  (-0.1 *. (x ** 3.)) +. (2. *. (x ** 2.)) -. (8. *. x) -. 10.
;;

assert (minmaxfun f 0 10 = Some (f 2, f 10));;
assert (minmaxfun f (-2) 10 = Some (f 2, f (-2)));;
assert (minmaxfun f (-2) 12 = Some (f 2, f (-2)));;
assert (minmaxfun f 0 20 = Some (f 20, f 11))
