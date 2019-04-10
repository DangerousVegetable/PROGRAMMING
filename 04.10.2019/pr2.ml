open String;;
type tree = N of int * tree * tree|L;;

let rec parse_node s n = 
let parse_int n = 
(int_of_string (sub s n ((index_from s n ',') - n)),(index_from s n ',')) in 

match s.[n] with
'(' -> let (num,i) = parse_int (n+1) in (); let (t1,i1) = parse_node s (i+1) in (); if s.[i1] <> ',' then failwith (string_of_int i1) else let (t2,i2) = parse_node s (i1+1) in (); if s.[i2] <> ')' then failwith (string_of_int i2) else (N (num,t1,t2),i2+1)
|_ -> if s.[n+1] <> ')' && s.[n+1] <> ',' then failwith (string_of_int n) else (L,n+1);; 

let parse s = 
let (t,i) = parse_node s 0 in if i = length s then t else failwith ((string_of_int i)^"gg");;


let bfs tr = 
let rec f que l = 
match que with
[] -> l
|L::tl -> f tl l 
|N (i,t1,t2)::tl -> f (tl@[t1;t2]) (l@[i]) in
f [tr] [];;

let print_list l = 
List.iter (fun x -> Printf.printf "%i;" x) l;;

let tr = parse (read_line());;

print_list (bfs tr);;

