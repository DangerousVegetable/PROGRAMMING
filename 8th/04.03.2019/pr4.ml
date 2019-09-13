open String;;
open Array;;

let n = read_int ();;

let max_height = n+1;;
let max_weight = n+1;;

let m = init max_weight (fun _ -> init max_height (fun _ -> "  "));;

let drw_ () =                                                 
for y = 0 to (max_height-1) do 
	(for x = 0 to (max_weight) do (
	if x = (max_weight) then print_char '\n' else (print_string (m.(x)).(y)));	 
	done);
done;; 


let draw_ln (x1,y1) (x2,y2) ch = 
let rec f n k b = 
if (x1 <= n && n <= x2) || (x2 <= n && n <= x1) then let ny = (int_of_float(k*.(float_of_int n) +. b)) in if ny < max_height then ((m.(n)).(ny) <- ch; f (n+1) k b) else f (n+1) k b else if (n < max_weight) then f (n+1) k b in
if x1 = x2 then (
	if y1 <= y2 then 
	for y = y1 to y2 do 
	(m.(x1)).(y) <- ch 
	done else (
	for y = y1 downto y2 do 
	(m.(x1)).(y) <- ch 
	done)) else (  
let k = ((float_of_int y2)-.(float_of_int y1))/.((float_of_int x2)-.(float_of_int x1)) in
let b = (float_of_int y1) -. k*.(float_of_int x1) in 
f 0 k b);;

let draw_king n (x,y) ch = if n mod 2 = 0 then failwith "N%2 = 0 ?!!" else 
draw_ln (x,y) (x+n-1,y) ch;
draw_ln (x+(n-1)/2,y) (x+(n-1)/2,y+n-1) ch; 
draw_ln (x,y+(n-1)/2) (x+n-1,y+(n-1)/2) ch; 
draw_ln (x,y+n-1) (x+n-1,y+n-1) ch;;

draw_king n (0,0) "+ ";; 
drw_();;
                  

