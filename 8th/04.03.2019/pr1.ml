open String;;

let print_arrows n = 
let rec f k = 
if k < n then (let s = make k ' ' in print_string (s^"*\n"); f (k+1)) else if k < ((2*n)-1) then (let s = make (n-(k-n)-2) ' ' in print_string (s^"*\n"); f (k+1)) in
f 0;;

print_arrows (read_int());;