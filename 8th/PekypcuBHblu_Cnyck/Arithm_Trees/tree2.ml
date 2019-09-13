open String;;



type tree = Sum of tree * tree| Mul of tree * tree| Min of tree*tree | Div of  tree*tree| L of int;;

let s = " "^(read_line());;

let rec parse_s n = 
let (t1,m1) = parse_m n in 
	match s.[m1] with 
	 '+' -> let (t2,m2) = parse_s (m1-1) in (Sum (t2,t1),m2)
	|'-' -> let (t2,m2) = parse_s (m1-1) in (Min (t2,t1),m2)
	|_ -> (t1,m1) and

parse_m n = 
let (t1,m1) = parse_t n in 
	match s.[m1] with 
	 '*' -> let (t2,m2) = parse_m (m1-1) in (Mul (t2,t1),m2)
	|'/' -> let (t2,m2) = parse_m (m1-1) in (Div (t2,t1),m2)
	|_ -> (t1,m1) and
	   
parse_t n = 
let rec givenum pos start = 
try let _ = int_of_string (make 1 (s.[pos])) in givenum (pos-1) start 
	with e -> (*if s.[pos] = '-' then (let numb = int_of_string (sub s (pos+1) (start-pos)) in (-numb,pos-1)) else*) (let numb = int_of_string (sub s (pos+1) (start-pos)) in (numb,pos)) in

match s.[n] with 
')' -> let (tr,m1) = parse_s (n-1) in (tr,m1-1)           
 |a -> let (num,pos) = givenum n n in (L num,pos);;


let rec print_tree k = 
match k with 
 Sum (t1,t2) -> print_char '('; print_tree t1; print_char '+'; print_tree t2; print_char ')';
|Mul (t1,t2) -> print_char '('; print_tree t1; print_char '*'; print_tree t2; print_char ')'; 
|Min (t1,t2) -> print_char '('; print_tree t1; print_char '-'; print_tree t2; print_char ')';
|Div (t1,t2) -> print_char '('; print_tree t1; print_char '/'; print_tree t2; print_char ')'; 
|L a -> print_int a;;

let (tr,_) = (parse_s ((length s)-1));;

print_tree tr;;