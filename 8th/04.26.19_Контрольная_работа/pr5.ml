open String;;

type t = Br of t*t|L;;


let rec parse_t s n =
match s.[n] with
 'L' -> (L,n+1)
|'B' -> let (t1,n1) = parse_t s (n+3) in (); if s.[n1] = ',' then let (t2,n2) = parse_t s (n1+1) in if s.[n2] = ')' then ((Br (t1,t2)),n2+1) else failwith (string_of_int n2) else failwith (string_of_int n1)
|_ -> failwith (string_of_int n);;

let parse s = 
let (t,n) = parse_t s 0 in if n = (length s) then t else failwith (string_of_int n);;

let rec to_str_t t = 
match t with
 L -> "L"
|Br (t1,t2) -> "Br(" ^ (to_str_t t1) ^ "," ^ (to_str_t t2)^")";;

print_string (to_str_t (parse (read_line())));;

