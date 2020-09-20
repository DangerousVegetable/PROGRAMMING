let l = List.init (read_int()) (fun x -> x+1);;

print_float ((float_of_int (List.fold_left (fun s x -> let f = string_of_int x in if f.[0] = '1' then s+1 else s) 0 l))/.(float_of_int (List.length l)));;