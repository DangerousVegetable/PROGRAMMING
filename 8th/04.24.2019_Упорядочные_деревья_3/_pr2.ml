open String;;
open Array;;                                                            


type ('a,'b) tree = N of ('a * 'b) * ('a,'b) tree * ('a,'b) tree | L;;

(*вот сама функция -> *)
let rec make_tree l =
	let cm (a,_) (b,_) = if a = b then 0 else if a > b then 1 else -1 in 

	let rec cut li n = if n = 0 then ([],li) else     
	match li with
	|[] -> failwith "wr_CUT"
	|a::b -> let (l1,l2) = cut b (n-1) in (a::l1,l2) in  
 
	let rec f li n = if n = 0 then L else let (l1,l2) = cut li (n/2) in N (List.hd l2,(f l1 (n/2)),(f (List.tl l2) (n - n/2 - 1))) in
f (List.stable_sort cm l) (List.length l);;

let t = make_tree [(1,4);(4,6);(7,3);(2,43);(11,74);(5,20); (* (8,1);(18,28);(-4,78);*) (16,-3);(-9,33);(59,12);(3,7);(20,-94);(17,9)];;



(*это для красивого рисования*)

let rec weight_tree tr = 
match tr with
L -> (2,1)
|N ((k1,k2),t1,t2) -> let (w1,n1) = (weight_tree t1) in (); let (w2,n2) = (weight_tree t2) in (); ((String.length (string_of_int k1))+2+(String.length (string_of_int k2))+w1+w2,(max n1 n2)+1);;
          
let (w,n) = weight_tree t;;

let max_height = 2*n+1;;
let max_weight = w;;

let m = init max_weight (fun _ -> init max_height (fun _ -> " "));;
                                                                       

let drw_ () =                                                 
for y = 0 to (max_height-1) do 
	(for x = 0 to (max_weight) do (
	if x = (max_weight) then print_char '\n' else (print_string (m.(x)).(y)));	 
	done);
done;; 

let draw_tree t = 
let rec dr_str s n x y = 
if n >= (String.length s) then () else ((m.(x+n)).(y) <- (String.make 1 s.[n]); dr_str s (n+1) x y) in 

let num = ref 0 in
let rec draw_tr tr y = 
match tr with
L -> (m.(!num)).(y) <- "!"; num := !num+2; (!num-2,!num-2)
|N ((k1,k2),tr1,tr2) -> let str1 = (string_of_int k1) in (); let len1 = (String.length str1) in (); let str2 = (string_of_int k2) in (); let len2 = (String.length str2) in (); let (_,nl2) = (draw_tr tr1 (y+2)) in dr_str str1 0 (!num) y; dr_str str2 0 (!num+len1+1) y; (m.(!num+len1)).(y) <- "=";  num := !num + (len1 + len2 + 2); let (nr1,_) = (draw_tr tr2 (y+2)) in (m.(nl2)).(y+1) <- "/"; (m.(nr1)).(y+1) <- "\\"; (nl2,nr1) in
draw_tr t 0;;


draw_tree t;;
drw_();;



