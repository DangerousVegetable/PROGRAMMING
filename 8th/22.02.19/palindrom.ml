open String;;

let mostpalin s = 
let rec maxpal1 n k = 
if (n - k) >= 0 && (n+k) < (length s) then if s.[n-k] = s.[n+k] then (maxpal1 n (k+1)) else (n-(k-1),n+(k-1)) else (n-(k-1),n+(k-1)) in

let rec maxpal2 n k = 
if (n - k) >= 0 && (n+1+k) < (length s) then if s.[n-k] = s.[n+k+1] then (maxpal2 n (k+1)) else (n-(k-1),n+1+(k-1)) else (n-(k-1),n+1+(k-1)) in 

let rec main n (st,fin) = if n >= length s then (st,fin) else let (x1,y1) = maxpal1 n 1 in (); let (x2,y2) = maxpal2 n 1 in 
if (y1 - x1) >= (y2 - x2) then if (y1 - x1) >= (fin - st) then main (n+1) (x1,y1) else main (n+1) (st,fin) else if (y2 - x2) >= (fin - st) then main (n+1) (x2,y2) else main (n+1) (st,fin) in
main 0 (0,0);;

let s = read_line();;

let (x,y) = mostpalin s;;                               

Printf.printf "OT:%i DO:%i" x y;;