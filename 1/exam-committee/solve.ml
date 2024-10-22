type vote =
  | StrongReject
  | WeakReject
  | WeakAccept
  | StrongAccept

let decide_exam : vote -> vote -> vote -> bool =
  fun a b c ->
    match a, b, c with
    | StrongReject, _, _
    | _, StrongReject, _
    | _, _, StrongReject -> false
    (* no one casted a [StrongReject] *)
    | WeakReject, WeakReject, _ 
    | WeakReject, _, WeakReject 
    | _, WeakReject, WeakReject -> false
    (* no one casted a [WeakReject] *)
    | _ -> true