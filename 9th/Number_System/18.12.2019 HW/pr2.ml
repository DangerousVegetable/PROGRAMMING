let string_from_3_ l = List.fold_left (fun s x -> if x = -1 then s ^ "!" else s ^ (string_of_int x)) "" (List.rev l);;    

print_string (string_from_3_ [-1;0;-1;1]);; (*== 17*)                  
