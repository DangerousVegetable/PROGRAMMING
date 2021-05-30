let s = read_line();;

let reg = Str.regexp "\\(\\([1-9]\\([0-9]*\\)\\)\\|0\\)\\.[0-9]+$";;

if Str.string_match reg s 0
then print_string "Matched!\n"
else print_string "Match failed!\n" ;;