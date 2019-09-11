let rec f n = if n >= 12 then 0. else  
	match n with 
	|n when n = 0|| n = 2|| n = 4|| n = 6|| n = 7|| n = 9|| n = 11 -> 31. +. (f (n+1)) 
	|n when n = 1 ->  29.+.(f (n+1)) 
	|_ -> 30. +. (f (n+1));;

print_float ((f 0)/.12.);; 