open String;;
type tree = N of int * tree * tree|L;;


let rec restore klp lkp = 

let rec to_fst x l =
match l with
[] -> []
|a::b when a = x -> []
|a::b -> a::(to_fst x b) in

let cut l st len = 
let rec ct x l = if x >= st + len then [] else 
match l with
 [] -> []
|a::b -> if x >= st then a::(ct (x+1) b) else ct (x+1) b in
ct 0 l in

let depth = List.length klp in
 
match klp with
 [] -> L
|k::lp -> let llkp = (to_fst k lkp) in (); let ln = (List.length llkp) in (); let lklp = (cut lp 0 ln) in (); let plkp = (cut lkp (ln+1) (depth - ln -1)) in (); let pklp = (cut lp ln (depth - ln  - 1)) in let trl = restore lklp llkp in (); let trp = restore pklp plkp in N(k,trl,trp);;

let rec tr_to_str tr = 
match tr with
 L -> "-"
|N (k,tr1,tr2) -> "("^(string_of_int k)^","^(tr_to_str tr1)^","^(tr_to_str tr2)^")";;


print_string (tr_to_str (restore [1;3;5;7;6](*KLP*) [3;1;7;5;6](*LKP*) ));; 
   