let mem x s = List.exists (( = ) x) s
let rec dup = function [] -> false | x :: t -> mem x t || dup t

let rec helper1 l s =
  match l with [] -> s | x :: t -> helper1 t (if mem x t then s else x :: s)

let rec helper2 l s =
  match l with [] -> s | x :: t -> helper2 (List.filter (( <> ) x) t) (x :: s)

let mkset1 l = helper1 l []
let mkset2 l = helper2 l []
let mkset = mkset2

let rec union1 s1 s2 =
  match s1 with
  | [] -> s2
  | x :: t -> if mem x s2 then union1 t s2 else x :: union1 t s2

let union2 s1 s2 = mkset (s1 @ s2) (* tail recursive *)
let union = union2
let inter s1 s2 = List.filter (fun x -> mem x s2) s1
let diff s1 s2 = List.filter (fun x -> not (mem x s2)) s1;;

assert (diff [ 2; 4; 5 ] [ 1; 2; 3; 4 ] = [ 5 ])

let dsum s1 s2 = List.map (fun x -> (0, x)) s1 @ List.map (fun x -> (1, x)) s2

let rec powset = function
  | [] -> [ [] ]
  | x :: t ->
      let ps = powset t in
      List.map (fun s -> x :: s) ps @ ps

let powset_tr s =
  let rec helper s ps =
    match s with
    | [] -> ps
    | x :: t -> helper t (List.map (fun s' -> x :: s') ps @ ps)
  in
  helper s [ [] ]

(* Annotate powset with type int list -> int list list to trace on int sets *)
