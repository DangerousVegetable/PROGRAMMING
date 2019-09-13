open Graphics;;
open Array;;
open_graph "640x480";;

set_line_width 2;;


let draw_pere pere = 
	
	let len = length pere in 
	let rec what_r rn n = if n >= length pere then rn else (
	let k = String.length (string_of_int pere.(n)) in if k > rn then (what_r k (n+1)) else (what_r rn (n+1))) in

	let rec _line x y m n r = if n >= length m then () else (draw_circle x y r; moveto x (y-5); draw_string (string_of_int m.(n)); _line (x+2*r) y m (n+1) r) in 

	let rec arrow (x1,y1) (x2,y2) lgth = moveto x1 y1; lineto x2 y2; let rad = sqrt (float_of_int((x1-x2)*(x1-x2))+.float_of_int((y1-y2)*(y1-y2))) in (); let cs = (float_of_int(x2-x1))/.rad in (); let ss = (float_of_int(y2-y1))/.rad in 
	moveto x1 y1; lineto (int_of_float((float_of_int x1) +. lgth *. (sqrt 2.) /. 2. *. (cs +. ss))) (int_of_float((float_of_int y1) +. lgth *. (sqrt 2.) /. 2. *. (-.cs +. ss)));  
        moveto x1 y1; lineto (int_of_float((float_of_int x1) +. lgth *. (sqrt 2.) /. 2. *. (cs -. ss))) (int_of_float((float_of_int y1) +. lgth *. (sqrt 2.) /. 2. *. (cs +. ss)));
        moveto x2 y2; lineto (int_of_float((float_of_int x2) +. lgth *. (sqrt 2.) /. 2. *. (-.cs +. ss))) (int_of_float((float_of_int y2) +. lgth *. (sqrt 2.) /. 2. *. (-.cs -. ss)));  
        moveto x2 y2; lineto (int_of_float((float_of_int x2) +. lgth *. (sqrt 2.) /. 2. *. (-.cs -. ss))) (int_of_float((float_of_int y2) +. lgth *. (sqrt 2.) /. 2. *. (cs -. ss))) in

	let rec draw_arrows n x y1 y2 r = if n >= length pere then () else (arrow (x + 2*r*n,y1) (x + (2*r*(pere.(n))),y2) 5.; draw_arrows (n+1) x y1 y2 r) in  

	let r = (what_r 1 0) in ();
	let sortpere = copy pere in
	(sort compare sortpere);

	let x = (320 - 10*r*len) in  
	let y1 =  (240 + 20*r) in 
	let y2 = (240 - 20*r) in 

	_line x y1 sortpere 0 (r*10); 
	_line x y2 pere 0 (r*10);
	draw_arrows 0 x y1 y2 (r*10);; 
                                   
draw_pere [|0;2;1;3;7;6;5;4|];;

read_line();;


  
	
	              