let n = read_int ();;

let a = Array.make n [];;
let b = Array.make n None;;

for i = 0 to (n-1) do
	let s = read_line() in
	a.(i) <- List.fold_left (fun l x -> let x = int_of_string x in if x <> 0 then (x-1)::l else l) [] (String.split_on_char ' ' s);
done;;

let rec give_number m num = 
	match b.(m) with
	|Some _ -> num
	|None -> let newnum = List.fold_left (fun num m -> give_number m num) num a.(m) in b.(m) <- Some newnum; newnum+1;;

for i = 0 to (n-1) do	
	give_number i 0;
done;;

let bnew = (Array.mapi (fun i (Some x) -> (i,x)) b);;

Array.sort (fun (_,i) (_,j) -> compare j i) bnew;;

Array.iter (fun (x,_) -> Printf.printf "%d " (x+1)) bnew;;

 
	