let rec f n y = if y > 2999 then 0. else if n >= 12 then (f 0 (y+1)) else  
	match n with 
	|n when n = 0|| n = 2|| n = 4|| n = 6|| n = 7|| n = 9|| n = 11 -> 31. +. (f (n+1) y) 
	|n when n = 1 -> if (y mod 400 = 0)||((y mod 4 = 0)&&(y mod 100 <> 0)) then 29.+.(f (n+1) y) else 28.+.(f (n+1) y)
	|_ -> 30. +. (f (n+1) y);;

print_float ((f 0 2000)/.12./.1000.);; 