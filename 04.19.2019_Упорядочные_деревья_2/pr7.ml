open String;;
type tree = N of int * tree * tree|L;;

let rec parse_node s n = 
let parse_int n = 
(int_of_string (sub s n ((index_from s n ',') - n)),(index_from s n ',')) in 

match s.[n] with
'(' -> let (num,i) = parse_int (n+1) in (); let (t1,i1) = parse_node s (i+1) in (); if s.[i1] <> ',' then failwith (string_of_int i1) else let (t2,i2) = parse_node s (i1+1) in (); if s.[i2] <> ')' then failwith (string_of_int i2) else (N (num,t1,t2),i2+1)
|_ -> if s.[n+1] <> ')' && s.[n+1] <> ',' then failwith (string_of_int n) else (L,n+1);; 

let parse s = 
let (t,i) = parse_node s 0 in if i = length s then t else failwith ((string_of_int i)^"parse");;


let rec tr_to_str_lkp tr = 
match tr with
 L -> ""
|N (k,tr1,tr2) -> "("^(tr_to_str_lkp tr1)^","^(string_of_int k)^","^(tr_to_str_lkp tr2)^")";;

let rec tr_to_str_klp tr = 
match tr with
 L -> "_"
|N (k,tr1,tr2) -> "("^(string_of_int k)^","^(tr_to_str_klp tr1)^","^(tr_to_str_klp tr2)^")";;

let rec fstgrtr tr k = 
match tr with
 L -> failwith "TyT"
|N (x,tr1,tr2) -> if x <= k then fstgrtr tr2 k else x;;  

let t = parse (read_line());;

print_int (fstgrtr t (read_int()));;
