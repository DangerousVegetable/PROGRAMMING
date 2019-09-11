let rec f1 a = 
if a > 0 then f2 (a-1) else 0 and 
	f2 a = 
if a>0 then f3 (a-1) else 1 and
	f3 a = 
if a>0 then f1 (a-1) else 2;;


print_int(f1 (read_int ()));;
