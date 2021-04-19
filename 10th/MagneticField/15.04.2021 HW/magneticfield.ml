let width = 100;;
let height = 100;;

let screen = Array.init width (fun i -> Array.make height " ");;

let dots = [(25.,50.,1.); (75.,50.,1.); (50., 25., 1.); (50., 75., -.1.);];;

let norm (x,y) = (x/.((x*.x +. y*.y)**0.5),y/.((x*.x +. y*.y)**0.5));;

let place x y c = 
	if x >= 0 && x < width && y >= 0 && y < height then screen.(x).(y) <- c;;

let rec anim p =
	let rec f p = 
		match p with
		|[] -> []
		|(x,y,q)::tl -> let (fx,fy) = norm (List.fold_left (fun (fx,fy) (qx,qy,qq) -> let r = (x-.qx)*.(x-.qx) +.(y-.qy)*.(y-.qy) in let (dx,dy) = norm ((x-.qx),(y-.qy)) in let (nx,ny) = (qq*.q*.1.*.dx/.r,qq*.q*.1.*.dy/.r) in (fx+.nx,fy+.ny)) (0.,0.) dots) in
					(*Printf.printf "force: %f,%f\n" fx fy;*)
					let (nx,ny) = (x+.fx,y+.fy) in
					let (dx,dy) = (int_of_float nx, int_of_float ny) in
					let (ox,oy) = (int_of_float x, int_of_float y) in
					if dx > ox && dy > oy then (place ox oy "/");
					if dx < ox && dy > oy then (place ox oy "\\");
					if dx = ox && dy > oy then (place ox oy "|");
					if dx > ox && dy = oy then (place ox oy "-");
					if dx < ox && dy = oy then (place ox oy "-");
					if dx > ox && dy < oy then (place ox oy "\\");
					if dx < ox && dy < oy then (place ox oy "/");
					if dx = ox && dy < oy then (place ox oy "|");
					(x+.fx,y+.fy, q)::(f tl) in
								
	let l = ref p in
	for i = 0 to 10000 do l := f !l; done;
	List.iter (fun (x,y,q) -> if q = 1. then place (int_of_float x) (int_of_float y) "+" else place (int_of_float x) (int_of_float y) "*") dots;;
	
(*anim [(25.,65.,1.)];;*)	

let rad = 1.;;
let pi = 3.1415926535897932384626433832795;;
let num = 16.;;

let qs = List.fold_left (@) [] (List.map (fun (x,y,q) -> if q = 1. then List.init (int_of_float num) (fun i -> let alp = (2.*.pi/.num)*.(float_of_int i) in (x+.(cos alp)*.rad, y+.(sin alp)*.rad, 1.)) else []) dots);;
	
anim qs;;	
	
for j = height-1 downto 0 do
	for i = 0 to width-1 do
	if i = 0 || i = width-1 || j = 0 || j = height -1 then (print_string "% ") else
	print_string (screen.(i).(j)^" ");
	done; print_string "\n"; done;;
	
					





