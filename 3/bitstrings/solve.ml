type bitstring = E | Z of bitstring | U of bitstring

let rec string_of_bitstring = function
  | E -> ""
  | Z bs -> "0" ^ string_of_bitstring bs
  | U bs -> "1" ^ string_of_bitstring bs

let rec bitstring_of_string s =
  String.fold_right
    (fun c acc ->
      match c with '0' -> Z acc | '1' -> U acc | _ -> failwith "not a bit")
    s E

let rec len = function E -> 0 | Z bs | U bs -> 1 + len bs
let rec countZ = function E -> 0 | Z bs -> 1 + countZ bs | U bs -> countZ bs
let rec countU = function E -> 0 | Z bs -> countU bs | U bs -> 1 + countU bs

let rec concat b1 b2 =
  match b1 with E -> b2 | Z bs -> Z (concat bs b2) | U bs -> U (concat bs b2)

let rec equals b1 b2 =
  match (b1, b2) with
  | E, E -> true
  | Z b1', Z b2' | U b1', U b2' -> equals b1' b2'
  | _ -> false

let tail = function E -> E | Z bs | U bs -> bs

let rec prefix b1 b2 =
  match (b1, b2) with
  | E, _ -> true
  | Z b1', Z b2' | U b1', U b2' -> prefix b1' b2'
  | _ -> false

let rec substring b1 b2 =
  match (b1, b2) with
  | _ when len b1 > len b2 -> false
  | Z _, Z b2' | U _, U b2' -> prefix b1 b2 || substring b1 b2'
  | Z _, U b2' | U _, Z b2' -> substring b1 b2'
  | _ -> false
