open String;;
type tree = Comma of (tree*tree)| Star;;

let rec parse_comma p s = match s.[p] with
 '*' -> (Star,p+1)
|'(' -> let (left,p1) = parse_comma (p+1) s in if s.[p1] <> ',' then failwith (string_of_int p1) else let (right,p2) = parse_comma (p1+1) s in if s.[p2] <> ')' then failwith (string_of_int p2) else (Comma (left,right),p2+1)
|_ -> failwith (string_of_int p);;


let parse s = 
let (t,p) = parse_comma 0 s in if p <> (length s) then failwith (string_of_int p) else t;;

parse (read_line());;                