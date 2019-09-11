open String;;
type tree = N of int * tree * tree|L;;


let rec make_whl n t = 
if n = 0 then L else let num = (1 lsl (n-1)) in N (num + t, make_whl (n-1) t, make_whl (n-1) (t + num));;

let rec tr_to_str_lkp tr = 
match tr with
 L -> ""
|N (k,tr1,tr2) -> "("^(tr_to_str_lkp tr1)^","^(string_of_int k)^","^(tr_to_str_lkp tr2)^")";;


print_string (tr_to_str_lkp (make_whl (read_int()) 0));; 


