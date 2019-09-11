open String;;                                                               

type opti = O of char | Non;;

let rec gen s n = 
if n = 0 then "" else s^(gen s (n-1));;

let rec plus n= 
Printf.printf "%s\n" (gen "+-----" n);;

let rec diff l ls= 
match l with 
[] -> ls
|(O(c),_,_)::b -> if List.exists (fun x -> x = O(c)) ls then diff b ls else diff b (O(c)::ls)
|(Non,_,_)::b -> if List.exists (fun x -> x = Non) ls then diff b ls else diff b (Non::ls);;

let rec ramka l = 
	Printf.printf "%s" "      ";
		let rec r l=
		match l with 
		[] -> ()
		|O(c)::b -> Printf.printf "|   %c " c; r b
		|Non::b -> Printf.printf  "%s" "|   *\n" in
	r l; 
plus ((List.length l)+1);; 


let rec avto l l2 ll n= 
match (l,l2) with 
([],_) -> ()
|((c,i,i2)::b,a::b2) -> if n = 0 then (Printf.printf "  %3i " i; avto l l2 ll ((n+1) mod (List.length ll + 1))) else if a = c then (Printf.printf "| %3i " i2; avto b b2 ll ((n+1) mod (List.length ll + 1))) else (Printf.printf "%s" "|     "; avto l b2 ll ((n+1) mod (List.length ll + 1))) 
|(k,[]) -> print_string "\n"; plus ((List.length ll)+1); avto k ll ll 0;; 


let ($<) (c1,i11,i12) (c2,i21,i22) =
if i11 < i21 then true else if i11 = i21 then (
match (c1,c2) with 
(O(a),O(a2)) -> (Char.code a)<(Char.code a2)
|(_,Non) -> true
|(Non,_) -> false
) else false;;


let rec supirsort l f=
match l with 
[] -> []
|[a] -> [a]
|a::b -> (supirsort (List.fold_left (fun y x -> if (f x a) || x = a then y@[x] else y) [] b) f)@[a]@(supirsort (List.fold_left (fun y x -> if (f a x) then y@[x] else y) [] b) f);;

let rec findsost i c l =  
match l with 
(O(a),i1,i2)::b -> if i1 = i && c = a then i2 else findsost i c b
|(Non,i1,i2)::b -> if i1 = i then i2 else findsost i c b
|[] -> failwith "";;  


let rec funct i s n l=
if n >= (length s) then (
	if i = 1 then (true,true) 
		else if i = 2 then (true,false) 
			else (false,false)
)else (funct (findsost i s.[n] l) s (n+1) l);;

                                                                                      

let rec result s l= 
let q = supirsort l ($<) in ();
let g = supirsort (diff q []) (fun x y -> (x,0,0) $< (y,0,0)) in
ramka g;
avto q g g 0; 
print_string "\n";
match funct 0 s 0 (q) with 
(true,true) -> print_string "OK +"
|(true,false) -> print_string "OK -"                                                                                                          
|(false,false) -> print_string "NOT OK";;  

let rec f a b = 
[(O(a),0,3);(O(b),0,4);(Non,0,0);(Non,1,1);(O(a),3,3);(O(b),3,1);(Non,3,3);(O(a),4,1);(O(b),4,4);(Non,4,4)];;

let s = read_line();;
let (a,b) = ((read_line()).[0],(read_line()).[0]);;


result s (f a b);;

