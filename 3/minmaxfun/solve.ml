let minmaxfun (f : int -> 'a) (a : int) (b : int) : ('a * 'a ) option =
  let rec go (now : int) : ('a * 'a ) option =
    if now <= b then
      let fnow = f now in
      match go (now + 1) with
      | Some (min, max) -> 
        Some (
          (if fnow < min then fnow else min),
          (if fnow > max then fnow else max))
      | None -> Some (fnow, fnow)
    else None
  in go a
;;



assert (minmaxfun (fun x -> x) (-2) 5 = Some (-2,5));;
assert (minmaxfun (fun x -> x) 5 (-2) = None);;
assert (minmaxfun (fun x -> x) 5 5 = Some (5,5));;
assert (minmaxfun (fun x -> x * x) (-2) 5 = Some (0,25));;

let curve x = x |> Float.of_int |> fun x -> x ** 3.0 -. 3.0 *. x;;
let arccos x = x |> Float.of_int |> Float.acos;;

assert (minmaxfun curve (-2) 2 = Some (-2.0,2.0));;
assert (minmaxfun arccos (-1) 1 = Some (0., Float.pi));;