let re = Str.regexp "[2-7]\\(0\\|2\\|4\\|6\\|8\\)$" ;;

if Str.string_match re (read_line()) 0
then print_string "Matched!\n"
else print_string "Match failed!\n" ;;