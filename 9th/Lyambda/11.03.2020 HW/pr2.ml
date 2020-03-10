open Array;;

open String;;


type l = Var of string| App of l*l| Abs of string*l;;

(*let related lya  = 
	let rec f l a =
		match l with
		|Var s -> if s = a then Var ("!"^a) else Var s
		|App (l1,l2) -> App (f (f l1 "") a,f (f l2 "") a)
		|Abs (s,l) -> Abs(s,f (f l s) a) in
	f lya ""                                                       
;; *)

let rec rel_list lya = 
	let rec f l a n = 
		match l with
		|Var s -> if s = a then ([n],n+(length s)) else ([],n+(length s))
		|App (l1,l2) -> let (ll,n1) = f l1 a (n+1) in let (lr,n2) = f l2 a (n1+1) in (ll@lr,n2+1)
		|Abs (s,l) -> if s = a then let (_,n1) = f l "" (2+(length s)+n) in ([],n1) else let (ll,n1) = f l a (2+(length s)+n) in (ll,n1) in
	let rec main l n =
		match l with
		|Var s -> ([],n+(length s))
		|App (l1,l2) -> let (ll,n1) = main l1 (n+1) in let (lr,n2) = main l2 (n1+1) in (ll@lr,n2+1)
		|Abs (s,l) -> let (ll,_) = f l s (2+(length s)+n) in let (lr,n2) = main l (2+(length s)+n) in ((s,n,ll)::lr,n2) in
	main lya 0;;  

let rec l_to_string l = 
	match l with 
 	|Var s -> s 
	|App (l1,l2) -> "("^(l_to_string l1)^" "^(l_to_string l2)^")"
	|Abs (s,l1) -> "\\"^s^"." ^ (l_to_string l1);; 


let print_rel lya = 
	
	let s_lya = (l_to_string lya) in

	let len = length s_lya in

	let (rel_list,_) = (rel_list lya) in

	Printf.printf "%s\n" s_lya;	
    
	let m = Array.make len " " in 
	List.iter (fun (_,n,l) -> m.(n) <- "^"; List.iter (fun n -> m.(n) <- "|") l) rel_list;                                                                                                   
	Array.iter (fun x -> print_string x) m; print_string "\n";

	let rec f l = 
		match l with
		|[] -> ()
		|(_,_,[])::tl -> f tl
		|(_,n0,li)::tl -> let maxn = (List.hd (List.rev li)) in let m = Array.make len " " in List.iter (fun (_,n,li) -> if n >= n0 && n <= maxn then m.(n) <- "+" else m.(n)<-"|"; List.iter (fun n -> if n >= n0 && n <= maxn then m.(n) <- "+" else m.(n)<-"|";) li) l; 
						  Array.iteri (fun i _ -> if i <= maxn && i>=n0 && m.(i) = " " then m.(i) <- "-") m;
						  Array.iter (fun x -> print_string x) m; print_string "\n"; f tl in
	 
	f rel_list;;					     
		

let lya = (Abs("y",Abs("xarr",Abs("t",App(Var "xarr",App(App(Var "t",Var "y"),Abs("xarr",App(Var "xarr",App(Var "t",Var "xarr")))))))));;

print_rel lya;;

(*let (l,_) = rel_list lya;; *)






(*List.iter (fun (s,n,l) -> if l = [] then () else Printf.printf "((%s,%n) <- [%s]);" s n (List.fold_left (fun str n -> str^(string_of_int n)^";") "" l)) l;; *)
                                                                                                                                                                

