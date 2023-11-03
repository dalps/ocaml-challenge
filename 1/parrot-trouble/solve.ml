let parrot_trouble talking = function
  | h when h >= 0 && h <= 23 -> Some (talking && (h <= 7 || h >= 20))
  | _ -> None
