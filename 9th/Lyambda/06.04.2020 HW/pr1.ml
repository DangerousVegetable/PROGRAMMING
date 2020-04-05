type lambda = Var of string| App of lambda*lambda| Abs of string*lambda;;

let rec l_to_string l = 
	match l with 
 	|Var s -> s 
	|App (l1,l2) -> "("^(l_to_string l1)^" "^(l_to_string l2)^")"
	|Abs (s,l1) -> "\\"^s^"." ^ (l_to_string l1);; 


let rec find_beta l =	
	match l with
	|App(Abs (_,l1),l2) -> l::((find_beta l1)@(find_beta l2))
	|Var _ -> []
	|App (l1,l2) -> ((find_beta l1)@(find_beta l2))
	|Abs (_,l) -> find_beta l;;

let lam = App (Abs ("x",Var "x"), App (Abs ("x",Var "x"),Abs ("x",Var "x")));;

List.iter (fun l -> Printf.printf "%s;" (l_to_string l)) (find_beta lam);;


