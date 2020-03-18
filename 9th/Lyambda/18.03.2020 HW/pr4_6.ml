let (||$) a b = a (fun f x -> f) (b (fun f x -> f) (fun f x -> x));;    (*OR*)

let (|$|) a b = a (b (fun f x -> x) (fun f x -> f)) (b (fun f x -> f) (fun f x -> x));; (*XOR*) 

let not a = a (fun f x -> x) (fun f x -> f);; (*NOT*)

let tt = fun f x -> f;;
let tf = fun f x -> x;;

(*print_string ((tf |$| tt) "True" "False");; *)

print_string ((not tf) "True" "False");;
