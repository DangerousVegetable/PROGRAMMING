open Hashtable;;

let hf s n =
	let rec f m = 
		if m < 0 then 0 else (((Char.code s.[m]) + 11*(f (m-1))) mod n) in
	f ((String.length s)-1);;

let tb = Hashtable.create 10 hf;;
Hashtable.add tb "a" 6;;
add tb "ab" 10;;
let Some v = add tb "a" 1;;
Printf.printf "%d\n" v;; 
Printf.printf "%b\n" (mem tb "a");;
let Some v = find tb "a";;
Printf.printf "%d\n" v;; 
let Some v = delete tb "a";;
Printf.printf "%d\n" v;;
Hashtable.add tb "b" 3;;
iter tb (fun (a,b) -> Printf.printf "(%s,%d)" a b);;
print_string "\n";;
Printf.printf "%d\n" (fold tb (fun s (_,b) -> s+b) 0);;  