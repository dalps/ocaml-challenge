type student = {
  id : string;
  name : string;
  surname : string;
  vote : int option;
  laude : bool;
}

let id_of_noshow =
  List.filter_map (fun s ->
      Option.fold ~none:(Some s.id) ~some:(fun _ -> None) s.vote)

let upgradeable =
  List.filter_map (fun s ->
      Option.fold ~none:None
        ~some:(fun v -> if v >= 15 && v <= 17 then Some s.surname else None)
        s.vote)

let upgrade =
  List.map (fun s ->
      if s.vote >= Some 15 && s.vote <= Some 17 then
        {
          id = s.id;
          name = s.name;
          surname = s.surname;
          vote = Some 18;
          laude = false;
        }
      else s)

let wrong_laude =
  List.filter_map (fun s ->
      if s.laude && s.vote < Some 30 then Some s.surname else None)

let fix_laude sl =
  List.map
    (fun s ->
      if List.mem s.surname (wrong_laude sl) then
        {
          id = s.id;
          name = s.name;
          surname = s.surname;
          vote = s.vote;
          laude = false;
        }
      else s)
    sl

let percent_passed sl =
  let n = List.length sl in
  let c = List.filter (fun s -> s.vote >= Some 18) sl |> List.length in
  c * 100 / n

let avg_vote sl =
  let n = List.length sl in
  let c =
    List.fold_left
      (fun acc s ->
        acc
        +
        if s.vote >= Some 18 then Option.get s.vote
        else 0 + if s.laude then 2 else 0)
      0 sl
  in
  c / n
