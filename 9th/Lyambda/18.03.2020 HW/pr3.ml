let is_Zero n = n (fun _ -> fun f x -> x) (fun f x -> f);;

let zero = fun f x -> x;;
let one = fun f x -> f x;; 

print_string ((is_Zero one) "True" "False");;
