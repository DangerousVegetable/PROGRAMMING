let f n =
	let ar = Array.init (2*n+1) (fun _ -> Array.make (n+1) 0) in
	ar.(2*n).(0) <- 1;

	let rec g p h =
		if p < 0 then () else
		if h < 0 then g (p-1) n else( 
			(if h = 0 then ar.(p).(h) <- ar.(p+1).(h+1)
				else if h = n then ar.(p).(h) <- ar.(p+1).(h-1) 
					else ar.(p).(h) <- (ar.(p+1).(h+1) + ar.(p+1).(h-1)));
			g p (h-1)) in   
g (2*n-1) n;
(*for i = 0 to 2*n do
	for j = 0 to n do
		Printf.printf "%d " ar.(i).(j);
	done;
	print_string "\n";
done;*)
ar.(0).(0);;

print_int (f 15);;
