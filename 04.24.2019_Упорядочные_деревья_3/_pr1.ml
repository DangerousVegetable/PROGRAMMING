open String;;

type ('a,'b) tree = N of 'a * 'b * ('a,'b) tree * ('a,'b) tree | L;;

let rec find tr vl = 
match tr with
|L -> None
|N (a,b,t1,t2) -> if vl < a then find t1 vl else if vl > a then find t2 vl else (Some b);;

let tr  = L;;
let v = 0;;
(*
match (find tr v) with
|None -> print_string "No elements have been found"
|Some k -> print_int k;;     *)
