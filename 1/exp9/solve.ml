let square x = x * x

let exp9 n = n |> square |> square |> square |> ( * ) n (* 4 multiplications *)