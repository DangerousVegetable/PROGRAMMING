let n = read_int ();; 

let rec f n = 
	if n = 0 then 1. else ((365.-.(float_of_int n)+.1.)/.365.)*.(f (n-1));;                        

print_float (if n >= 365 then 1. else 1. -. (f n));; 