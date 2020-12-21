type ('a,'b) t = ('a*'b) list ref;;

let create _ _ = ref [];; 

let add table a b = 
	let (l,p) = List.fold_left (fun (s,p) (m,n) -> if p = None then if m = a then (s,Some n) else ((m,n)::s,None) else ((m,n)::s,p)) ([],None) !table in	
	table:=(a,b)::l;                                
	p;;
 
let mem table a = 
	List.exists (fun (k,v) -> k = a) !table;;

let find table a = 
	let t = List.find_opt (fun (k,v) -> k = a) !table in
	match t with
	|None -> None
	|Some (_,v) -> Some v;;            

let delete table a = 
	let (l,p) = List.fold_left (fun (s,p) (m,n) -> if p = None then if m = a then (s,Some n) else ((m,n)::s,None) else ((m,n)::s,p)) ([],None) !table in
	table:=l;
	p;;

let iter table f = 
	List.iter f !table;;                        

let fold table f s = 
	List.fold_left f s !table;;                       
