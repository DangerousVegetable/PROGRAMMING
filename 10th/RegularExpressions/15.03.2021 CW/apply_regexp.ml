open Parse;;
open Regexp_graph;;

let apply_regexp s reg = 
	let (g,_,start,_) = reg_graph reg in

	let rec check q n = 
		match g.(q) with
		|[] -> if n = String.length s then true else false
		|l -> List.fold_left (fun b (nq,cond) -> if b then b else	
								match cond with
								|None -> check nq n
								|Some c -> if n < String.length s then 
										if s.[n] = c then check nq (n+1)
										else b
									   else b) false l in
	check start 0;;

let reg = parse "[A-Z]|[a-z]";;
print_string (string_of_reg reg);;
print_string "\n";;
                                 
let s = "q";;

Printf.printf "%b" (apply_regexp s reg);;
