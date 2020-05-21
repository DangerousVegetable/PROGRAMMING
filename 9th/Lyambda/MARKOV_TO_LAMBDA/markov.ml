open Parse_lambda;;
open Beta_reduction;;
open String;;

type rule = T of (string*string)|U of (string*string);;

let test l = print_string (l_to_string (beta_reduction (parse_l l)));;

let y = "(\\f.(\\x.f (x x)) (\\x.f (x x)))";;

let t = "(\\f.\\x.f)";;
let f = "(\\f.\\x.x)";;

let makePair = "(\\a.\\b.\\f.f a b)";;

let fst = "(\\a.(a "^t^"))";;
let scd = "(\\a.(a "^f^"))";;

let iF = "(\\x.\\a.\\b.x a b)";;

let plusOne = "(\\n.\\f.\\x.f (n f x))";;
let plus = "(\\a.\\b.a "^plusOne^" b)";;
let mul = "(\\a.\\b.a ("^plus^" b) (\\f.\\x.x))" 

let minusOne = "(\\n.(n (\\a."^makePair^" (a "^f^") ("^plusOne^" (a "^f^"))) ("^makePair^" (\\f.\\x.x) (\\f.\\x.x)))"^t^")";; 
let minus = "(\\a.\\b.b "^minusOne^" a)";;

let iF = "(\\x.\\a.\\b.x a b)";;

let greq = "(\\a.\\b.("^minus^" b a) (\\f."^f^") "^t^")";;
let gr = "(\\a.\\b."^greq^" a ("^plusOne^" b))";;
let eq = "(\\a.\\b.("^greq^" a b) ("^greq^" b a) "^f^")";;


let inl = "(\\p.\\f.\\g.f p)";;
let inr = "(\\p.\\f.\\g.g p)";;
let case = "(\\p.\\f.\\g.p f g)";;
let hd = parse_l ("(\\l."^case^" l (\\x.*) (\\p.p "^t^"))");;
let tl = parse_l ("(\\l."^case^" l (\\x.*) (\\p.p "^f^"))");;
let concat = "(\\f.\\l1.\\l2."^case^" l1 (\\x.l2) (\\p."^inr^" ("^makePair^" (p "^t^") (f (p "^f^") l2))))";;


(*let try_replace = "(\\f.\\s.\\r."^case^" (r "^t^" "^t^") (\\x."^makePair^" (("^y^" "^concat^") (r "^t^" "^f^") s) "^t^") (\\p."^case^" s (\\x."^makePair^" s "^f^") (\\q."^iF^" ("^eq^" (p "^t^") (q "^t^")) (f (q "^f^") ("^makePair^" ("^makePair^" (p "^f^") (r "^t^" "^f^")) (r "^f^"))) ("^makePair^" s "^f^")))";; 
*)

let try_replace = "(\\f.\\s.\\r."^case^" (r "^t^" "^t^") (\\x."^makePair^" ("^y^" "^concat^" (r "^t^" "^f^") s) "^t^") (\\p."^case^" s (\\x."^makePair^" s "^f^") (\\q."^iF^" ("^eq^" (p "^t^") (q "^t^")) (f (q "^f^") ("^makePair^" ("^makePair^" (p "^f^") (r "^t^" "^f^")) (r "^f^"))) ("^makePair^" s "^f^"))))";;

let apply = "(\\f.\\s.\\r.(\\a."^iF^" (a "^f^") a ("^case^" s (\\x."^makePair^" s "^f^") (\\p.(\\b."^makePair^" ("^inr^" ("^makePair^" (p "^t^") (b "^t^"))) (b "^f^")) (f (p "^f^") r)))) ("^y^" "^try_replace^" s r))";;

let step = "(\\f.\\s.\\r."^case^" r (\\x."^makePair^" s "^t^") (\\p.(\\a."^iF^" (a "^f^") ("^makePair^" (a "^t^") (p "^t^" "^f^")) (f s (p "^f^"))) ("^y^" "^apply^" s (p "^t^"))))";;

let algorythm = "(\\f.\\s.\\r.(\\a."^iF^" (a "^f^") (a "^t^") (f (a "^t^") r)) ("^y^" "^step^" s r))";;

let rec make_list sl = 
	match sl with
	|[] -> "("^inl^" _"^")"
	|s::tl -> "("^inr^" ("^makePair^" "^s^" "^(make_list tl)^"))";;

(*let string_to_lambda_list s = 
	let rec f n = 
		if n >= String.length s then [] else (String.make 1 s.[n])::(f (n+1)) in
	make_list (f 0);;  *)
	
let int_to_l n = 
	let rec f n = 
		if n = 0 then "x" else "f ("^(f (n-1))^")" in
	"(\\f.\\x."^(f n)^")";;

let string_to_l s =
	let rec f n = 
		if n >= length s then [] else (int_to_l ((Char.code s.[n]) - 32))::(f (n+1)) in
	(make_list (f 0));;

let rec l_to_list l = 
	match (beta_reduction (App(hd,l))) with
	|Var "*" -> []
	|a -> a::(l_to_list (beta_reduction (App(tl,l))));;

let l_to_int l =
	let rec f l =	
		match l with	
		|App(_,l) -> 1 + (f l)
		|_ -> 0 in
	
	match l with
	|Abs(a,Abs(b,l)) -> f l
	|_ -> failwith "incorrect church";;

let ll_to_string ls = List.fold_left (fun s x -> s^(make 1 (Char.chr x))) "" (List.map (fun l -> l_to_int l + 32) (l_to_list ls));;                                            
                                                                       
(*print_string (ll_to_string (string_to_l "12345Ad hey am i workin'?"));;*)

let rule_to_l rl =
	let rec g rl = 
		match rl with
		|[] -> []
		|(T (s1,s2))::tl -> ("("^makePair^" ("^makePair^" ("^(string_to_l s1)^") ("^(string_to_l s2)^")) "^t^")")::(g tl)
		|(U (s1,s2))::tl -> ("("^makePair^" ("^makePair^" ("^(string_to_l s1)^") ("^(string_to_l s2)^")) "^f^")")::(g tl) in
	make_list (g rl);; 		 
	                                                                                                         
(*List.iter (fun x -> print_string ((l_to_string x)^" ;")) (l_to_list (parse_l (rule_to_l [U ("!","\"");T ("\"","!")])));;*)

let rl = rule_to_l [U ("a","b");T ("b","c")];;    (*правила U - нетерминальные, T - терминальные*)
let str = string_to_l "ba";;     (*строка*)

let markov = "("^y^" "^algorythm^" "^str^" "^rl^")";;

(*let test_concat = ("("^y^""^concat^" ("^inl^" _) ("^(string_to_l "!")^"))");;

let out = open_out "OUT";;

Printf.fprintf out "%s" (l_to_string (parse_l test_concat));;

print_string (ll_to_string (parse_l test_concat));;  *)

(*let test_step = "("^y^" "^step^" "^str^" "^rl^") "^t^"";;  

print_string (ll_to_string (beta_reduction (parse_l test_step)));; *)

let out = open_out "OUT";;

Printf.fprintf out "%s" (l_to_string (parse_l markov));;    (*ƒЋя  ќћѕ»Ћя÷»» - ocamlc parse_lambda.ml beta_reduction.ml markov.ml -o markov.exe*)



