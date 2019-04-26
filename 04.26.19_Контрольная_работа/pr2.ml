open String;;

type t = Br of t*t|L;;

let to_str tr n =
let rec f k t s = 
match t with
 L -> s^(make (k*n) ' ')^"."^"\n"
|Br (t1,t2) -> (f (k+1) t2 (f (k+1) t1 (s^(make (k*n) ' ')^"*"^"\n"))) in
f 0 tr "";;

print_string (to_str (Br (Br(L,Br(L,L)),L)) (read_int()));;


