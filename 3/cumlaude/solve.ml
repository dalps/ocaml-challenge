type grade = Val of int | CumLaude

let is_valid = function Val n -> n >= 18 && n <= 30 | _ -> true

let int_of_grade = function
  | Val n as v when is_valid v -> n
  | CumLaude -> 32
  | _ -> failwith "not a grade"

let avg grades =
  let sum, len =
    List.fold_left
      (fun acc g -> (fst acc + int_of_grade g, snd acc + 1))
      (0, 0) grades
  in
  float_of_int sum /. float_of_int len
