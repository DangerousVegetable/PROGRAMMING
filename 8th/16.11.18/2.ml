let rec f l x = 
match l with 
[] -> 0
|(a,b,c)::q when a = x && b = x && c = x -> 3 + (f q x)
|(a,b,c)::q when a = x && b = x -> 2 + (f q x)
|(a,b,c)::q when a = x && c = x -> 2 + (f q x)
|(a,b,c)::q when b = x && c = x -> 2 + (f q x)
|(a,b,c)::q when a = x -> 1 + (f q x)
|(a,b,c)::q when b = x -> 1 + (f q x)
|(a,b,c)::q when c = x -> 1 + (f q x)
|_::q -> f q x;;

let rec ret l x = 
(f l x) mod 2 = 1;;  

 