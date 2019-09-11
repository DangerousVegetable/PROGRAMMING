open Array;;

let swap i j a = 
if a.(i) <= a.(j) then (let x = a.(j) in a.(j) <- a.(i); a.(i) <- x; false) else (let x = a.(j) in a.(j) <- a.(i); a.(i) <- x; true);; 

let pr_arr a =
Array.iter (fun x -> Printf.printf "%i;" x) a;;

let sdwig a i j =
for k = j downto (i+1) do 
swap k (k-1) a; done; a;; 

let razb a x =
let rec f left right = if left = right then a else
if a.(left) <= x then f (left+1) right else (swap right left a; f left (right-1)) in
f 0 ((length a)-1);;  

let copyy i j a b= 
for k = i to j do
b.(k)<-a.(k); done;;


(*pr_arr (razb [|5;5;5;5;5;5;5;5;5;4;4;4;4;4;4;4;4|] 4);;   *)                 
                                                          
