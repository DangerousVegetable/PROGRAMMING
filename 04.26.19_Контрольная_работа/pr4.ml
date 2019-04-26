open String;;
type t = B of t list;;

let rec to_str t n=  
let rec f l k = 
match l with
 [] -> "\n"
|(B li)::tl -> (make (k*n) ' ')^"B [\n"^(f li (k+1))^(make (k*n+2) ' ')^"];\n"^(f tl k) in
f [t] 0;;

print_string (to_str (B[B[B[];B[]];B[];B[]]) (read_int()));; 
