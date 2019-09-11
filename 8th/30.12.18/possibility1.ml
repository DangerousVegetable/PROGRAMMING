type pos = Non|Alw|Just of int;;
type som = N|S of char;;

let (&) a b = 
match (a,b) with
(_,Non) -> Non
|(f,Alw) -> f
|(Just x,Just y) -> if x = y then (Just x) else Non
|(Alw,f) -> f
|_ -> Non;;

let rec find_l l a =
let rec find l a n= 
match l with 
[] -> -1
|x::b when x = a -> n 
|_::b -> find b a (n+1) in 
find l a 0;;

let print_pos a = 
match a with
 Non -> Printf.printf "None\n"
|Alw -> Printf.printf "Always\n"
|Just x -> Printf.printf "Just at %i\n" x;;

let rec find_l_s l a = 
let rec fin l a q = 
match l with 
[] -> List.hd (List.rev q)  
|(S x, n)::b -> if x = a then n else fin b a q
|(N,n)::b -> fin b a (n::q) in
fin l a [];;                                                           


print_pos ((Just 20)&(Just 20));;
print_int (find_l_s [(S 'f',4);(N,8);(S '1',3)] '1');;


