let rec f n k = 
if n = 1 then [] else
if n mod k = 0 then (k :: (f (n/k) k)) else (f n (k+1));;

List.iter (fun x -> Printf.printf "%i; " x) (f (read_int()) 2);;
