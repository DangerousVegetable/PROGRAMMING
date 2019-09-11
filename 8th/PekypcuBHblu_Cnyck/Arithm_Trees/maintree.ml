open String;;


type tree = Emp|Sum of tree * tree| Mul of tree * tree| Min of tree*tree | Div of  tree*tree| Pow of tree*tree| Eq of tree*tree| L of int;;

let s = (read_line())^";";;

let rec parse_e n tre = 
if tre = Emp then (let (t1,m1) = parse_s n Emp in parse_e m1 t1) else 
 (match s.[n] with
|'=' -> let (t1,m1) = parse_s (n+1) Emp in parse_e m1 (Eq (tre,t1))
|_ -> (tre,n))
and

parse_s n tre = 
if tre = Emp then (let (t1,m1) = parse_m n Emp in parse_s m1 t1) else
 (match s.[n] with
 '+' -> let (t1,m1) = parse_m (n+1) Emp in parse_s m1 (Sum (tre,t1))
|'-' -> let (t1,m1) = parse_m (n+1) Emp in parse_s m1 (Min (tre,t1))
|_ -> (tre,n))
and

parse_m n tre = 
if tre = Emp then (let (t1,m1) = parse_st n in parse_m m1 t1) else 
 (match s.[n] with
 '*' -> let (t1,m1) = parse_st (n+1) in parse_m m1 (Mul (tre,t1))
|'/' -> let (t1,m1) = parse_st (n+1) in parse_m m1 (Div (tre,t1))
|_ -> (tre,n))
and

parse_st n = 
let (t1,m1) = parse_t n in
if s.[m1] = '^' then (let (t2,m2) = parse_st (m1+1) in (Pow (t1,t2),m2)) else (t1,m1) and

parse_t n = 
let rec givenum pos num = 
try let nu = int_of_string (make 1 (s.[pos])) in givenum (pos+1) (num*10+nu) 
	with e -> (num,pos) in

match s.[n] with 
'(' -> let (tr,m1) = parse_e (n+1) Emp in (tr,m1+1)
 |a -> let (num,pos) = givenum n 0 in (L num,pos);;


let rec print_tree k = 
match k with 
 Sum (t1,t2) -> print_char '('; print_tree t1; print_char '+'; print_tree t2; print_char ')';
|Mul (t1,t2) -> print_char '('; print_tree t1; print_char '*'; print_tree t2; print_char ')'; 
|Div (t1,t2) -> print_char '('; print_tree t1; print_char '/'; print_tree t2; print_char ')'; 
|Min (t1,t2) -> print_char '('; print_tree t1; print_char '-'; print_tree t2; print_char ')'; 
|Pow (t1,t2) -> print_char '('; print_tree t1; print_char '^'; print_tree t2; print_char ')'; 
|Eq (t1,t2) -> print_char '('; print_tree t1; print_char '='; print_tree t2; print_char ')'; 
|Emp -> ()
|L a -> print_int a;;


let count_tree tre = 
let rec count tr = 
match tr with 
 Sum (t1,t2) -> let (a1,b1,c1) = count t1 in let (a2,b2,c2) = count t2 in (a1 || a2, b1 && b2, c1+c2)
|Mul (t1,t2) -> let (a1,b1,c1) = count t1 in let (a2,b2,c2) = count t2 in (a1 || a2, b1 && b2, c1*c2) 
|Div (t1,t2) -> let (a1,b1,c1) = count t1 in let (a2,b2,c2) = count t2 in (a1 || a2, b1 && b2, c1/c2)
|Min (t1,t2) -> let (a1,b1,c1) = count t1 in let (a2,b2,c2) = count t2 in (a1 || a2, b1 && b2, c1-c2)
|Pow (t1,t2) -> let (a1,b1,c1) = count t1 in let (a2,b2,c2) = count t2 in (a1 || a2, b1 && b2, int_of_float((float_of_int c1)**(float_of_int c2))) 
|Eq (t1,t2) -> let (a1,b1,c1) = count t1 in let (a2,b2,c2) = count t2 in if a1 && a2 then (true,b1 = b2,0) else if (not a1) && (not a2) then (true,c1 = c2,0) else (true,false,0) 
|Emp -> (true,true,0)
|L a -> (false,true,a) in
match (count tre) with
(true,b,_) -> Printf.printf "\n%b" b
|(false,_,c) -> Printf.printf "\n%i" c;;



let (tr,_) = (parse_e 0 Emp);;

print_tree tr;;
count_tree tr;;