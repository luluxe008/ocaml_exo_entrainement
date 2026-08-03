(* https://ocaml.org/exercises *)


let print opt = match opt with 
    | None -> print_string "None"
    | Some a -> print_string a

let rec last u = match u with
    | [] -> None
    | [ x ] -> Some x
    | _ :: x -> last x;;

let as_string = let x = last [] in 
Option.map string_of_int x;;

print as_string;;

let rec last_two u = match u with
    | [] | [_] -> None
    | [a; b] -> Some (a; b)
    | _ :: t -> last_two t

let rec at i u = match u with
| [] -> None
| x :: v -> if i = 0 then (Some x) else at (i-1) v;;

print_newline ();;

let str = Option.map string_of_int (at  2 [1; 5; 8]);;
let () = print str;;