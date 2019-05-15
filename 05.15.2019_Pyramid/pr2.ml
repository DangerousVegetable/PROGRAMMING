open Array;;

let check_pyr m f = 
    let rec pyr n p = if n >= length m then true else if f p (m.(n)) then (pyr (2*n + 1) m.(n))&&(pyr (2*n + 2) m.(n)) else false in
pyr 0 m.(0);;

let what_pyr m =
	if check_pyr m (<=) then print_string "<=;" else
	if check_pyr m (>=) then print_string ">=;" else print_string "NOTHING";;
what_pyr [|1;2;3;5;6;3;2|];;                                                     