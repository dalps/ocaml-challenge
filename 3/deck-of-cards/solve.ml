type suit = Spades | Hearts | Diamonds | Clubs
type n = Card of int * suit

let partition deck =
  let rec helper ((s, h, d, c) as suits) = function
    | [] -> suits
    | Card (n, Spades) :: t -> helper (n :: s, h, d, c) t
    | Card (n, Hearts) :: t -> helper (s, n :: h, d, c) t
    | Card (n, Diamonds) :: t -> helper (s, h, n :: d, c) t
    | Card (n, Clubs) :: t -> helper (s, h, d, n :: c) t
  in
  helper ([], [], [], []) deck

let is_complete deck =
  let s, h, d, c = partition deck in
  let test ns = List.sort compare ns = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
  test s && test h && test d && test c

let extract i l =
  let rec helper carry n l =
    match l with
    | [] -> failwith "nth"
    | h :: t when n = i -> (h, carry @ t)
    | h :: t -> helper (carry @ [ h ]) (n + 1) t
  in
  helper [] 0 l

let gen_deck () =
  let init suit = List.init 10 (fun n -> Card (n + 1, suit)) in
  let pool = init Spades @ init Hearts @ init Diamonds @ init Clubs in
  let rec helper p acc =
    match p with
    | [] -> acc
    | _ ->
        let i = Random.int (List.length p) in
        let c, p' = extract i p in
        helper p' (c :: acc)
  in
  helper pool []
;;

assert (gen_deck () |> is_complete);;
