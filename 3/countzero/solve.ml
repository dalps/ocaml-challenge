let countzero f a b =
  let rec loop i =
    if i <= b then
      (if f i = 0 then 1 else 0) + loop (i+1)
    else
      0
  in loop a
;;

assert (countzero (fun x -> x) (-10) 10 = 1);;
assert (countzero (fun x -> x) 1 10 = 0);;
assert (countzero (fun x -> x*x - 1) (-10) 10 = 2);;
assert (countzero (fun x -> (if x<0 then -x else x) - 1) (-10) 10 = 2);;

(* #########
  Advanced solution for the curious, using a mutable state 
*)

type ('s,'a) state = 's -> 's * 'a

let pure (x : 'a) : ('s,'a) state = fun s -> (s,x)

let ( >>= ) (result : ('s,'a) state) (next : 'a -> ('s,'b) state) : ('s,'b) state =
fun s -> let s',x = result s in next x s'

let incr : (int,unit) state = fun n -> (n+1,())

let countzero' f a b =
  let rec loop i =
    if i > b then pure () else loop (i+1) >>= fun _ ->
    if f i = 0 then incr else pure ()
  in loop a 0 |> fst
;;

assert (countzero' (fun x -> x) (-10) 10 = 1);;
assert (countzero' (fun x -> x) 1 10 = 0);;
assert (countzero' (fun x -> x*x - 1) (-10) 10 = 2);;
assert (countzero' (fun x -> (if x<0 then -x else x) - 1) (-10) 10 = 2);;