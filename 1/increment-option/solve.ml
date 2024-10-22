let incr_opt : int option -> int option =
  function
  | None -> None
  | Some x -> Some (x + 1)