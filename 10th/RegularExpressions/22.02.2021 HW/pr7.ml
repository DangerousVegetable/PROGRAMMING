let s = read_line();;

let reg = Str.regexp 
("\\([a-z]+\\)://"^
"\\(\\([a-z]+\\)\\.\\([a-z]+\\)\\.\\([a-z]+\\)\\)"^
"\\(:\\(\\([0-9]\\)\\|\\([1-9][0-9]+\\)\\)\\)?"^
"\\(\\(/[a-zA-Z0-9]+\\)+\\(\\.[a-zA-Z0-9]+\\)?\\)?"^
"\\(\\?\\([a-zA-Z0-9%\\$@#_\\+-]+=[a-zA-Z0-9%\\$@#_\\+-]+\\)\\(&[a-zA-Z0-9%\\$@#_\\+-]+=[a-zA-Z0-9%\\$@#_\\+-]+\\)*\\)?"^
"\\(#.+\\)?$");;

if Str.string_match reg s 0
then print_string ("Matched!\nTop-level domain: "^(Str.matched_group 5 s))
	
else print_string "Match failed!\n" ;;