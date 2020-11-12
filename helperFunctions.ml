#use "pc.ml";;
open PC;;

let lowercase_ascii_list s  = List.map lowercase_ascii s ;;
let make_range_lowercase_ascii leq ch1 ch2 (s : char list) =
  const (fun ch -> (leq ch1 ch) && (leq ch ch2)) ( lowercase_ascii_list (s));;

let range_ci_lowercase_ascii =
  make_range_lowercase_ascii (fun ch1 ch2 ->
	      (lowercase_ascii ch1) <=
		(lowercase_ascii ch2));;

