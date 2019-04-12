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

let rec add tr x = 
match tr with
L -> N (x,L,L)
|N (k,t1,t2) -> if x >= k then N (k,t1,add t2 x) else N (k,add t1 x,t2);;

let rec tr_to_str tr = 
match tr with
 L -> "-"
|N (k,tr1,tr2) -> "("^(string_of_int k)^","^(tr_to_str tr1)^","^(tr_to_str tr2)^")";;

let tr = (parse (read_line()));;

print_string (tr_to_str (add tr (read_int())));;
