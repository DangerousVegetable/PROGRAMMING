open String;;
open Array;;


let n = read_int ();;

let max_height = 2*n + 1;;
let max_weight = 2*n + 1;;

let m = init max_weight (fun _ -> init max_height (fun _ -> "  "));;

let drw_ () =                                                 
for y = 0 to (max_height-1) do 
	(for x = 0 to (max_weight) do (
	if x = (max_weight) then print_char '\n' else (print_string (m.(x)).(y)));	 
	done);
done;; 

let draw_circle (x0,y0) r ch =
	for i = -r to r do 
	(let x = x0 + i in 
		let y = int_of_float((float_of_int y0)+.sqrt((float_of_int (r*r))-.(float_of_int (i*i)))) in
			if x < max_weight && y < max_height && x >= 0 && y >= 0 && (2*y0-y) < max_height && (2*y0-y) >= 0 then 
				((m.(x)).(y) <- ch; (m.(x)).(2*y0-y) <- ch) 
			else ())	
	done;;
                     
draw_circle (n,n) n "+ ";;
drw_();;


