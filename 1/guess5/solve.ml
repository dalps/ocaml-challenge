let guess5 n = match n with
| _ when n < 1 || n > 5 -> failwith "not valid"
| _ -> let r = (Random.int 5) + 1 in (n = r, r)