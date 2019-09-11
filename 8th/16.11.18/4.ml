open String;;

let isgeom s =
let rec check s n q= 
if n >= length s then true else let i1 =(Char.code(s.[n])) in (); let i2 = (Char.code(s.[n-1])) in if i1-i2 = q then check s (n+1) q else false in
if length s <= 1 then true else check s 1 ((Char.code (s.[1]))-(Char.code(s.[0])));;


Printf.printf "%b" (isgeom (read_line()));;


