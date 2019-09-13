open String;;

type t = Br of t*t|L;;


let rec parse_t s n k tab =
match s.[n] with
 '.' -> (L,n+1)
|'*' -> let (t1,n1) = parse_t s (n+2+(k+1)*tab) (k+1) tab in (); if s.[n1] = '\n' then let (t2,n2) = parse_t s (n1+1+(k+1)*tab) (k+1) tab in if s.[n2] = '\n' then ((Br (t1,t2)),n2) else failwith ("2 "^(string_of_int n2)) else failwith ("1 "^(string_of_int n1))
|_ -> failwith (string_of_int n);;

let parse s =           
let (t,n) = parse_t s 0 0 3 in if n >= ((length s)-1) then t else failwith ("End "^(string_of_int n));;

let rec to_str_t t = 
match t with
 L -> "L"
|Br (t1,t2) -> "Br(" ^ (to_str_t t1) ^ "," ^ (to_str_t t2)^")";;

let s = "*\n---*\n------.\n------*\n---------.\n---------.\n---.\n";;

print_string (to_str_t (parse s));;

