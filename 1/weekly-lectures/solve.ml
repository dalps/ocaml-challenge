type weekday = Mo | Tu | We | Th | Fr
type course = ALF | LIP

let isLecture d = function
  | ALF -> ( match d with Tu | Th | Fr -> true | _ -> false)
  | LIP -> ( match d with We | Th -> true | _ -> false)
