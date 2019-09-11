type proga = Let of string*proga*proga | Str of string | Seq of proga list;;

let rec print_proga tr = 
match tr with 
Let (s,tr1,tr2) -> print_string ("let "^s^" = "); print_proga tr1; (*print_string "\n";*) print_proga tr2
|Str s -> print_string s
|Seq l -> List.iter (fun x -> print_proga x; (*print_string "\n"*)) l;;

print_proga (Seq [(Str "(");(Let ("x",Str "5 in\n\t",(Let ("(z,y)",Str "('q','q') in ();",Str ""))));Str "\n);;"]);; 


