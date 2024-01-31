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

let length xs =
    let rec length' xs result =
        match xs with
        | [] -> result
        | _ :: rest -> length' rest (result + 1)
    in
    length' xs 0
;;

let rec rev input =
    let rec rev' input output =
        match input with
        | [] -> output
        | h :: t -> rev' t (h :: output)
    in
    rev' input []
;;

let () = List.iter print_string (rev [ "a"; "b"; "c"; "d"; "e"; "f"; "g" ])
