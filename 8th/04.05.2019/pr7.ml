open String;;

type comma = B of comma*comma| St;;

let rec parse_comma p s = match s.[p] with
 '*' -> (St,p+1)
|'(' -> let (left,p1) = parse_comma (p+1) s in if s.[p1] <> ',' then failwith (string_of_int p1) else let (right,p2) = parse_comma (p1+1) s in if s.[p2] <> ')' then failwith (string_of_int p2) else (B (left,right),p2+1)
|_ -> failwith (string_of_int p);;


let parse s = 
let (t,p) = parse_comma 0 s in if p <> (String.length s) then failwith (string_of_int p) else t;;



let rec draw_tree tr n dth l = 
let rec drw_st lis k = if k >= dth then () (*print_string "\n"*) else(  
match lis with 
|x::t when x = k -> print_string "|"; print_string (String.make n ' '); (*List.iter (fun x -> Printf.printf "%i;" x) tl;*) drw_st t (k+1)  
|_-> (print_string " "; print_string (String.make n ' '); (*List.iter (fun x -> Printf.printf "%i;" x) l;*) drw_st l (k+1))) in 

match tr with
 St -> drw_st l 0; print_string "|\n"; drw_st l 0; print_string ("+-@\n");  			(*for i = 1 to dth do print_string ((String.make (dth*n) ' ')^"|") done; print_string "\n"; for i = 1 to dth do print_string ((String.make (dth*n) ' ')^"|") done; print_string "+-@\n";  (*print_string ((String.make (dth*(n+1)) ' ' )^"|\n"); print_string ((String.make (dth*(n+1)) ' ' )^"+- *\n")*)*)
|B (tr1,tr2) -> drw_st l 0; print_string "|\n"; drw_st l 0; print_string ("+"^(make n '-')^"()\n"); draw_tree tr1 n (dth+1) (l@[dth+1]); draw_tree tr2 n (dth+1) l;; (*for i = 1 to dth do print_string ((String.make (dth*n) ' ')^"|") done; print_string "\n"; for i = 1 to dth do print_string ((String.make (dth*n) ' ')^"|") done; draw_tree tr1 n (dth+1); draw_tree tr2 n (dth+1);;*)


let tr = parse (read_line());;
draw_tree tr (read_int()) 0 [];;
