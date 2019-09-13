open Array;;

let check_pyr m f = 
    let rec pyr n p = if n >= length m then true else if f p (m.(n)) then (pyr (2*n + 1) m.(n))&&(pyr (2*n + 2) m.(n)) else false in
pyr 0 m.(0);;

let what_pyr m =
	if check_pyr m (<=) then print_string "<=;" else
	if check_pyr m (>=) then print_string ">=;" else print_string "NOTHING";;

let check_tree m f = 
	let rec tree n p = if n >= length m then (true,p,p) else (let (b1,mi1,ma1) = tree (2*n+1) (m.(n)) in let (b2,mi2,ma2) = tree (2*n+2) (m.(n)) in if b1&&b2 then if (f ma1 m.(n))&&(f m.(n) mi2) then (true,mi1,ma2) else (false,p,p) else (false,p,p)) in  
tree 0 m.(0);;

let what_tree m = 
let (b1,_,_) = check_tree m (<=) in
let (b2,_,_) = check_tree m (>=) in
	if b1 then print_string "<=;" else
	if b2 then print_string ">=;" else print_string "NOTHING";;

what_tree [|4;2;6;1;3;5;5|];;                                                     
