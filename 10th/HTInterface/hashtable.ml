type ('a,'b) t = {
	hashfunc: ('a -> int -> int);
	mutable data: ('a*'b) list array;	
};;

let create n hashf = {hashfunc = hashf; data = Array.make n []};; 
let add table a b =
	let hash = table.hashfunc a (Array.length table.data) in
	let l = table.data.(hash) in 
	let rec f l = 
		match l with
		|[] -> ([(a,b)],None)
		|(k,v)::tl -> if k = a then ((a,b)::tl,Some v) else let (tl,r) = f tl in ((k,v)::tl,r) in
	
	let (nl,r) = f l in
	table.data.(hash) <- nl;
	r;; 
	

let mem table a = 
	let hash = table.hashfunc a (Array.length table.data) in
	List.exists (fun (k,_) -> k = a) (table.data.(hash));;
	
let find table a = 
	let hash = table.hashfunc a (Array.length table.data) in
	let r = List.find_opt (fun (k,_) -> k = a) (table.data.(hash)) in
	match r with
	|None -> None
	|Some (_,b) -> Some b;;
	
let delete table a = 
	let hash = table.hashfunc a (Array.length table.data) in
	let l = table.data.(hash) in 
	let rec f l = 
		match l with
		|[] -> ([],None)
		|(k,v)::tl -> if k = a then (tl,Some v) else let (tl,r) = f tl in ((k,v)::tl,r) in
	
	let (nl,r) = f l in
	table.data.(hash) <- nl;
	r;;

let iter table f = 
	Array.iter (fun l -> List.iter f l) table.data;;

let fold table f s =     
	Array.fold_left (fun a l -> (List.fold_left f a l)) s table.data;;

