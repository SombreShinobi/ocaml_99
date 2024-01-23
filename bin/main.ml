let rec last xs =
    match xs with
    | [] -> None
    | [ x ] -> Some x
    | _ :: rest -> last rest
;;

let rec last_two xs =
    match xs with
    | [] -> None
    | [ x; y ] -> Some (x, y)
    | _ :: rest -> last_two rest
;;

let rec nth (xs : 'a list) (n : int) =
    match xs with
    | x :: _ when n == 1 -> Some x
    | _ :: rest when n > 1 -> nth rest (n - 1)
    | _ -> None
;;

let () =
    match nth [ "a"; "b"; "c"; "d"; "f"; "g" ] 1 with
    | Some x -> print_endline x
    | None -> print_endline "Ain't none"
;;
