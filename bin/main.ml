(* https://ocaml.org/exercises *)
(* Utop peut pas trouver les fonctions définis ici :'-[ *)

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

print_string "Le 2eme (2+1) élement de [1; 5; 8] est: "
let str = Option.map string_of_int (at  2 [1; 5; 8]);;
let () = print str;;


let rec lenght u = match u with
| [] -> 0
| _ :: v -> 1 + lenght v;;

(*presque *)
let rec duplicate l n = 
    let rec aux el n= match n with
    | 0 -> []
    | n ->  el :: aux el (n-1)
    in 
    match l with
    | [] -> []
    | x :: v -> aux x n :: duplicate v n;;


(* Syntax Checker *)
let identifier s= 
    let rec is_correct c = match c with 
    | 'a' .. 'z' | 'A' .. 'Z' | '-' | '0' .. '9' -> true
    | _ -> false
    in
    let all = String.for_all is_correct s
    in all && match String.get s 0 with 
        |  'a' .. 'z' | 'A' .. 'Z' -> true
        | _ -> false;;