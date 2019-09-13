open String;;



type tree = Sum of tree * tree| Mul of tree * tree| L of int;;

let s = (read_line())^" ";;

let rec parse_s n = 
let (t1,m1) = parse_m n in if s.[m1] = '+' then (let (t2,m2) = parse_s (m1+1) in (Sum (t1,t2),m2)) else (t1,m1) and

parse_m n = 
let (t1,m1) = parse_t n in if s.[m1] = '*' then (let (t2,m2) = parse_m (m1+1) in (Mul (t1,t2),m2)) else (t1,m1) and

parse_t n = 
let rec givenum pos num= 
try let nu = int_of_string (make 1 (s.[pos])) in givenum (pos+1) (num*10+nu) 
	with e -> (num,pos) in

match s.[n] with 
'(' -> let (tr,m1) = parse_s (n+1) in (tr,m1+1)
|'-'-> let (num,pos) = givenum (n+1) 0 in (L (-num),pos)
 |a -> let (num,pos) = givenum n 0 in (L num,pos);;


let rec print_tree k = 
match k with 
 Sum (t1,t2) -> print_char '('; print_tree t1; print_char '+'; print_tree t2; print_char ')';
|Mul (t1,t2) -> print_char '('; print_tree t1; print_char '*'; print_tree t2; print_char ')'; 
|L a -> print_int a;;

let (tr,_) = (parse_s 0);;

print_tree tr;;