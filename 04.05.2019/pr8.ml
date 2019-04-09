open String;;
open Array;;

type comma = B of comma*comma| St;;
type special = G of special*int*special*int|S;;

let rec parse_comma p s = match s.[p] with
 '*' -> (St,p+1)
|'(' -> let (left,p1) = parse_comma (p+1) s in if s.[p1] <> ',' then failwith (string_of_int p1) else let (right,p2) = parse_comma (p1+1) s in if s.[p2] <> ')' then failwith (string_of_int p2) else (B (left,right),p2+1)
|_ -> failwith (string_of_int p);;


let parse s = 
let (t,p) = parse_comma 0 s in if p <> (String.length s) then failwith (string_of_int p) else t;;

let rec max_depth t = 
match t with
St -> 1
|B (t1,t2) -> let a1 = max_depth t1 in (); let a2 = max_depth t2 in (a1+a2);;




let tr = parse (read_line());;

let n = max_depth tr;; 

let max_height = 3*n;;
let max_weight = n;;

let m = init max_weight (fun _ -> init max_height (fun _ -> " "));;

let drw_ () =                                                 
for y = 0 to (max_height-1) do 
	(for x = 0 to (max_weight) do (
	if x = (max_weight) then print_char '\n' else (print_string (m.(x)).(y)));	 
	done);
done;; 

let draw_tree t = 
let num = ref (-1) in
let rec draw_tr tr y = 
match tr with
St -> num := (!num+1); (m.(!num)).(y) <- "*"; (!num,!num)
|B (tr1,tr2) -> let (_,nl2) = (draw_tr tr1 (y+2)) in (); let (nr1,_) = (draw_tr tr2 (y+2)) in (m.(nl2)).(y) <- "("; (m.(nr1)).(y) <- ")"; (m.(nl2)).(y+1) <- "/"; (m.(nr1)).(y+1) <- "\\"; (nl2,nr1) in
draw_tr t 0;;

draw_tree tr;;
print_string "\n";;
drw_();;    


      

