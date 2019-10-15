let rec info_am l = 
	match l with 
	|[] -> 0.
	|a::b -> -. (a*.(log a)/.(log 2.)) +. info_am b;;

Printf.printf "%f" (info_am [1.;0.5;0.25]);;