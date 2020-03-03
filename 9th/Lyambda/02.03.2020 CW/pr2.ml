open String;;

type l = Var of string| App of l*l| Abs of string*l;;

let rec (=/) a b = 
        let rec repl a s1 s2 = 
		match a with
		|Var s -> if s = s1 then Var s2 else Var s
		|App (a1,a2) -> App(repl a1 s1 s2, repl a2 s1 s2)
		|Abs (s,a) -> if s = s1 then Abs(s2,repl a s1 s2) else Abs(s,a) in
	
	match (a,b) with
	|(Var a, Var b) -> a = b
	|(App (a1,a2), App (b1,b2)) -> (a1 =/ b1)&&(a2 =/ b2)
	|(Abs (s1,a), Abs (s2,b)) -> if s1 = s2 then a =/ b else (repl a s1 ("$"^s1)) =/ (repl b s2 ("$"^s1))
	|_ -> false;;

Printf.printf "%b" (App(Abs("a",Abs("a",Var "d")),Var "c") =/ App(Abs("b",Abs("b",Var "d")),Var "c"));;

