let ( <*> ) (f : ('a -> 'b) option ) (x : unit -> 'a option) : 'b option =
  match f with
  | None -> None
  | Some g ->
    match x () with
    | None -> None
    | Some y -> Some (g y)

let square x = x * x
let double x = 2 * x
let multiply x y = x * y;;

assert (Some square <*> (fun () -> None) = None);;
assert (None <*> (fun () -> Some 2) = None);;
assert (Some multiply <*> (fun () -> Some 3) <*> (fun () -> Some 2) = Some 6);;
assert (Some multiply <*> (fun () -> None) <*> (fun () -> Some 2) = None);;


let ( <|> ) (a : 'a option) (b : unit -> 'a option) : 'a option =
  match a with
  | None -> b ()
  | Some x -> Some x
;;

assert (Some true <|> (fun () -> Some false) = Some true);;
assert (None <|> (fun () -> Some false) = Some false);;
assert (Some 3 <|> (fun () -> None) = Some 3);;
assert (Some "cat" <|> (fun () -> print_endline "I won't be executed"; Some "dog") = Some "cat");;
assert (None <|> (fun () -> print_endline "Hello!"; Some "dog") = Some "dog");;
