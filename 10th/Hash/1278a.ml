let n = read_int();;
let a = Array.make n ("","");;

for i = 0 to (n-1) do
	let s1 = read_line() in
	let s2 = read_line() in
	a.(i) <- (s1,s2); done;;


let check a b =
	let b = "!"^b in 
	
	let rec h s n = 
		if n >= String.length s then 0 else (Char.code s.[n])+(h s (n+1)) in
	
	let to_a s =
		let a = Array.make (String.length s) 0 in
		
		let rec f n =	 
			if n>= String.length s then () else (a.(n) <- Char.code (s.[n]); f (n+1)) in 
		f 0; a in

	let srt s = let a = to_a s in Array.sort compare a; a in 	

	let hs = h a 0 in	
	let aa = srt a in 

        let rec is_shuffled s = aa = srt s in
	
	let rec f n hash =           
		if (String.length b - n) >= String.length a then 			                                      
				let newhash = hash - Char.code (b.[n-1]) + Char.code (b.[n-1 + (String.length a)]) in
				if newhash = hs && is_shuffled (String.sub b n (String.length a)) then true else f (n+1) newhash
		else false in
	if String.length b < String.length a then false else f 1 (h (String.sub b 0 (String.length a)) 0);;

(*Printf.printf "%b" (check "abc" "adfcdbaasjkdfhklasdfhklsj");; *)

(*let ans = Array.make n false;;
 *)
for i = 0 to (n-1) do 
	let (a,b) = a.(i) in
	if check a b then 
		print_string "YES\n"
	else print_string "NO\n" done;;
			                                               