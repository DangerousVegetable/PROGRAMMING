let rec f l x =
match l with 
[] -> []
|(a,b,x1)::q when x1 = x-> (a,b,x)::(f q x)
|(a,x1,b)::q when x1 = x -> (a,x,b)::(f q x)
|(x1,a,b)::q when x1 = x -> (x,a,b)::(f q x)
|_::q ->f q x;;