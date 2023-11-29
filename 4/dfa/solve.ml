module ListSet = struct
  let mem x s = List.exists (( = ) x) s
  let add x s = if mem x s then s else x :: s
  let add_opt o s = Option.fold ~none:s ~some:(fun x -> add x s) o

  let rec subseteq xl yl =
    match xl with [] -> true | x :: xl' -> mem x yl && subseteq xl' yl

  let seteq xl yl = subseteq xl yl && subseteq yl xl
  let rec dup = function [] -> false | x :: t -> mem x t || dup t

  let mkset l =
    let rec helper l s =
      match l with
      | [] -> s
      | x :: t -> helper (List.filter (( <> ) x) t) (x :: s)
    in
    helper l []

  let union s1 s2 = mkset (s1 @ s2)
  let inter s1 s2 = List.filter (fun x -> mem x s2) s1
  let diff s1 s2 = List.filter (fun x -> not (mem x s2)) s1
  let dsum s1 s2 = List.map (fun x -> (0, x)) s1 @ List.map (fun x -> (1, x)) s2

  let powset s =
    let rec helper s ps =
      match s with
      | [] -> ps
      | x :: t -> helper t (List.map (fun s' -> x :: s') ps @ ps)
    in
    helper s [ [] ]
end

type ('a, 'b) fsa = {
  trans : ('a * 'b * 'a) list; (* set of transitions *)
  init : 'a; (* initial state *)
  final : 'a list (* final states *);
}

open ListSet

let getlabels m =
  List.fold_left (fun acc (_, a, _) -> ListSet.add a acc) [] m.trans

let outlabels m q =
  List.fold_left
    (fun acc (q', a, _) -> if q = q' then ListSet.add a acc else acc)
    [] m.trans

let getstates m =
  List.fold_left (fun acc (q, _, r) -> add q acc |> add r) [] m.trans

let is_complete m =
  let states = getstates m in
  let labels = getlabels m in
  (* for every state q, all labels occur in the set of outgoing labels from q *)
  List.for_all (fun q -> subseteq labels (outlabels m q)) states

let is_deterministic1 m =
  let states = getstates m in
  List.for_all
    (fun q ->
      List.for_all
        (fun a ->
          List.filter (fun (q', a', _) -> q = q' && a = a') m.trans
          |> List.length |> ( = ) 1)
        (outlabels m q))
    states

let is_deterministic2 m =
  List.for_all
    (fun ((q, a, _) as t) ->
      List.exists (fun (q', a', _) -> q = q' && a = a') (diff m.trans [ t ])
      = false)
    m.trans

let is_deterministic = is_deterministic2

let step1 q a m =
  let _, _, r = List.find (fun (q', a', r) -> q = q' && a = a') m.trans in
  r

let step q w m = List.fold_left (fun q a -> step1 q a m) q w
let accept w m = mem (step m.init w m) m.final

let complete m sink =
  let states = getstates m in
  let labels = getlabels m in
  let newtrans =
    List.fold_left
      (fun acc q ->
        diff labels (outlabels m q)
        |> List.fold_left (fun acc' a -> (q, a, sink) :: acc') []
        |> ( @ ) acc)
      [] states
    (* sink state needs self loops! *)
    |> ( @ ) (List.fold_left (fun acc a -> (sink, a, sink) :: acc) [] labels)
  in
  { trans = union m.trans newtrans; init = m.init; final = m.final }
