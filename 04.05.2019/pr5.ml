open String;;
open Array;;

type comma = B of comma*comma| St;;

let rec parse_comma p s = match s.[p] with
 '*' -> (St,p+1)
|'(' -> let (left,p1) = parse_comma (p+1) s in if s.[p1] <> ',' then failwith (string_of_int p1) else let (right,p2) = parse_comma (p1+1) s in if s.[p2] <> ')' then failwith (string_of_int p2) else (B (left,right),p2+1)
|_ -> failwith (string_of_int p);;


let parse s = 
let (t,p) = parse_comma 0 s in if p <> (String.length s) then failwith (string_of_int p) else t;;
                     

(*let n = read_int ();;

let max_height = n+1;;
let max_weight = n+1;;

let m = init max_weight (fun _ -> init max_height (fun _ -> "  "));;

let drw_ () =                                                 
for y = 0 to (max_height-1) do 
	(for x = 0 to (max_weight) do (
	if x = (max_weight) then print_char '\n' else (print_string (m.(x)).(y)));	 
	done);
done;; *)


let rec draw_tree tr n dth = 
match tr with
 St -> print_string ((String.make (dth*n) ' ' )^"+\n")
|B (tr1,tr2) -> print_string ((String.make (dth*n) ' ')^"(\n"); draw_tree tr1 n (dth+1); draw_tree tr2 n (dth+1); print_string ((String.make (dth*n) ' ' )^"(\n");;


let tr = parse (read_line());;
draw_tree tr (read_int()) 0;;

