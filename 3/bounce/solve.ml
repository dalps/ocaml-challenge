let bounce (period : int) : int -> int =
  fun n -> 
    let step = n mod (2 * period) in
    if step < period then step else 2 * period - step