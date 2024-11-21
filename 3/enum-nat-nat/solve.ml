let rec enum_nat_nat : int -> int * int =
  function
  | 0 -> (0,0)
  | n -> let (a,b) = enum_nat_nat (n-1) in
    if b = 0 then (0,a+1) else (a+1,b-1)

