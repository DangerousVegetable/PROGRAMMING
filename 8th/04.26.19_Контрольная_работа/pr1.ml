type t = Br of t*t|L;;

let rec to_str_t t = 
match t with
 L -> "L"
|Br (t1,t2) -> "Br(" ^ (to_str_t t1) ^ "," ^ (to_str_t t2)^")";;

print_string (to_str_t (Br(Br(L,L),L)));;