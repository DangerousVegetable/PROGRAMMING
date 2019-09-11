type t = B of t list;;

let rec to_str t =  
let rec f l = 
match l with
 [] -> ""
|(B li)::tl -> "B ["^(f li)^"];"^(f tl) in
f [t];;

print_string (to_str (B[B[B[];B[]];B[];B[]]));; 
