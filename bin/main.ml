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

let rev xs =
    let rec rev' input output =
        match input with
        | [] -> output
        | h :: t -> rev' t (h :: output)
    in
    rev' xs []
;;

let is_palindrome xs = xs = rev xs

type 'a node =
    | One of 'a
    | Many of 'a node list

let rec flatten xs =
    let rec flatten' input output =
        match input with
        | [] -> output
        | One x :: rest -> flatten' rest (x :: output)
        | Many y :: rest -> flatten' rest (flatten' y output)
    in
    rev (flatten' xs [])
;;

let rec compress xs =
    match xs with
    | x :: (y :: _ as rest) -> if x = y then compress rest else x :: compress rest
    | smaller -> smaller
;;

(* let pack xs = *)
(*     match xs with *)
(*         | x :: (y:: _ as rest) -> if x = y then  *)
(**)
let () =
    List.iter print_string (compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])
;;
