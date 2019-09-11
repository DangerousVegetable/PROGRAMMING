open String;;
type tree = N of int * tree * tree|L;;

Random.self_init();;

let rec make_random n add = if n = 0 then (L,add,add) else  
	match (Random.int 2) with 
		 0 -> let (tr1,min1,max1) = make_random (n-1) add in (); let (tr2,min2,max2) = make_random (Random.int n) (max1 + 1) in (N (max1+1,tr1,tr2),min1,max2)
		|1 -> let (tr1,min1,max1) = make_random (Random.int n) add in (); let (tr2,min2,max2) = make_random (n-1) (max1 + 1) in (N (max1+1,tr1,tr2),min1,max2)


let rec tr_to_str_lkp tr = 
match tr with
 L -> ""
|N (k,tr1,tr2) -> "("^(tr_to_str_lkp tr1)^","^(string_of_int k)^","^(tr_to_str_lkp tr2)^")";;

let rec tr_to_str_klp tr = 
match tr with
 L -> "_"
|N (k,tr1,tr2) -> "("^(string_of_int k)^","^(tr_to_str_klp tr1)^","^(tr_to_str_klp tr2)^")";;

let n = (read_int());;

for i = 0 to (1 lsl n) do          
	let (rt,_,_) = (make_random n 0) in
	print_string (tr_to_str_lkp rt); 
	print_string ("\t \\\\"^(string_of_int i)^"\n"^"\n");
done;;
