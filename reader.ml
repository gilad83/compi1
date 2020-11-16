#use "pc.ml";;
open PC;;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;

type number =
  | Fraction of int * int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Fraction (n1, d1)), Number(Fraction (n2, d2)) -> n1 = n2 && d1 = d2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | _ -> false;;

module Reader: sig
  val read_sexprs : string -> sexpr list
end
= struct

let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;


(*helper functions  *)
let pair_to_bool (lst , _) =
  let  str = list_to_string(lst) in
  if str = "#t" || str = "#T" then true else false;;

let bool_of_string lst  =
  let  str = list_to_string(lst) in
  if str = "#t" || str = "#T" then true else false;;

(* Atomic Parsers *)
let comma = (char ',');;
let colon = (char ':');;
let dot = (char '.');;
let semi_colon = (char ';');;
let nt_no_new_line = const (fun ch -> ch != char_of_int 10);;
let nt_only_space = (char ' ' );;


(*Stared Atomic Parsers *)
let nt_star_whitespaces = star nt_whitespace;;
let nt_star_only_space = star (char ' ' );;


(* complex Parsers *)
let make_paired nt_left nt_right nt =
  let nt = caten nt_left nt in
  let nt = pack nt (function (_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt (function (e, _) -> e) in
  nt;;
let make_spaced nt =
  make_paired nt_star_whitespaces nt_star_whitespaces nt;;

let nt_line_comment = 
  let nt_end_of_line = (char (char_of_int 10)) in
  let comment_end = disj nt_end_of_line (pack nt_end_of_input (fun (e) -> 'e')) in
  let nt = caten  semi_colon (star nt_no_new_line) in
  let nt = caten nt comment_end in
  let nt = pack nt (fun e -> Nil) in
  nt;;

 let nt_symbol = 
  let nt_letters = disj (range 'A' 'Z')  (range 'a' 'z') in
  let upper_to_lower_case = pack nt_letters lowercase_ascii in
  let punct = disj_list [char '?';char '/';char '<';char '>';char '+';char '=' ;char '_' ;
    char '-';char '*';char '^';char '$';char '!';char ':';] in
  let digitG = range '0' '9' in
  let nt_no_dot = disj_list[digitG;upper_to_lower_case;punct] in
  let nt_with_dot = disj_list [dot;nt_no_dot] in 
  let nt_no_dot_start = caten nt_no_dot (star nt_with_dot) in
  let nt_dot_start = caten nt_with_dot (plus nt_with_dot) in
  let nt = disj_list [nt_no_dot_start;nt_dot_start] in
  let nt = pack nt (fun (c,e) -> let string_of_e = list_to_string e in
  let string_of_e = String.make 1 c ^ string_of_e in Symbol(string_of_e)) in 
  nt;;

let nt_boolean  = 
  let bool_tok =  not_followed_by (disj ( word_ci "#f")  (word_ci "#t")) nt_symbol  in
  pack bool_tok (fun (bool_t) -> Bool (bool_of_string (bool_t)));;

(* moved inside  *)
(************* number parsers *************)
let digit = range '0' '9';;
let digits = plus digit;;
let rec gcd a b = 
  if b = 0
  then a
  else gcd b (a mod b);;


let nt_natural = 
  pack digits (fun arr -> int_of_string (list_to_string arr));;

let nt_sign s = 
  let nt_s = const (fun ch -> ch = '-' || ch = '+') in
  let (sign, rest) = (maybe nt_s s) in
  match sign with
  | Some '-' -> (-1, rest)
  | Some '+' -> (1, rest)
  | _ -> (1, rest);;
  
let nt_signed_nat s = 
  let (sign, dgts) = (nt_sign s) in
  let (num, rest) = (nt_natural dgts) in
  (sign*num, rest);;
  
let nt_scientific_exp s = 
  let nt_e = const (fun ch -> ch = 'e' || ch = 'E') in
  let (e, rest) = (nt_e s) in
  (nt_signed_nat rest);;

let nt_integer s =
  let (num, rest) = (nt_signed_nat s) in
  try 
    let (exp, rest) = (nt_scientific_exp rest) in
    (Number(Float(float(num) *. 10.0 ** float(exp))), rest)
  with X_no_match -> (Number(Fraction(num, 1)), rest);;

let nt_fraction s =
  let nt_frac = const (fun ch -> ch = '/') in
  let (numerator, rest) = (nt_signed_nat s) in
  let (ch, rest) = (nt_frac rest) in
  let (denominator, rest) = (nt_signed_nat rest) in
  let n = gcd numerator denominator in
  (Number(Fraction(numerator/n, denominator/n)), rest);;

let nt_float s = 
  let (sign, rest) = (nt_sign s) in
  let (integer, mantissa) = (digits rest) in
  let (d, mantissa) = (dot mantissa) in
  let (mantissa, rest) = (digits mantissa) in
  let num = List.append integer (d::mantissa) in
  try
    let (exp, rest) = (nt_scientific_exp rest) in
    (Number(Float(float(sign)*.(float_of_string (list_to_string num)) *. 10.0 ** float(exp))), rest)
  with X_no_match -> (Number(Float(float(sign)*.(float_of_string (list_to_string num)))), rest);;

  
let nt_number = not_followed_by (disj nt_float (disj nt_fraction nt_integer)) nt_symbol;;



(* number parsers without combina *)
(* let nt_natural s = 
  pack digits (fun dgts -> List.fold_left (fun a b -> 10*a+(nt_digit b)) 0 dgts) s;;

let nt_mantissa s = 
  pack digits (fun dgts -> List.fold_right (fun a b -> float((nt_digit a)+b) /. 10.0) dgts 0) s;; *)

(************* end of number parsers *************)



(***************** String parser ******************)
let nt_meta_char = 
  let meta_chars = ["\\r"; "\\n"; "\\t"; "\\f"; "\\\\"; "\\\""] in
  let meta_pc_list = List.map word_ci meta_chars in
  let meta_pc = disj_list meta_pc_list in
  pack meta_pc (fun parsed -> 
    let expr = list_to_string (List.map lowercase_ascii parsed) in
    match expr with
    | "\\r" -> '\013'
    | "\\n" -> '\010'
    | "\\t" -> '\009'
    | "\\f" -> '\012'
    | "\\\\" -> '\092'
    | "\\\"" -> '\034'
    | _ -> '\000'
  );;

let nt_lit_char = 
  let nt_all_chars = const (fun ch -> true) in
  let nt_no_lit = disj (char '\"') (char '\\') in 
  diff nt_all_chars nt_no_lit;;

let nt_string_char = disj nt_meta_char nt_lit_char;;

let nt_string = 
  let d_qoute = char (char_of_int (34)) in
  let pc = star nt_string_char in
  let pc = caten d_qoute pc in
  let pc = caten pc d_qoute in
  pack pc (fun ((_,arr),_) -> String(list_to_string arr));;

(****************** End of string parser ******************)

(***************** Char parser ******************)
let vis_char = const (fun c -> (int_of_char c) > 32);;
let hashtag = (char '#');;
let double_slash = (char '\\');;
let prefix = caten hashtag double_slash;;
let name_char = disj_list [
  pack (word_ci "space") (fun e -> char_of_int 32);
  pack (word_ci "page") (fun e -> char_of_int 12);
  pack (word_ci "tab") (fun e -> char_of_int 9);
  pack (word_ci "return") (fun e -> char_of_int 13);
  pack (word_ci "newline") (fun e -> char_of_int 10);
  pack (word_ci "nul") (fun e -> char_of_int 0)] ;;
let nt_char = 
  let nt = (disj name_char vis_char ) in
  let nt = pack (caten prefix nt ) (fun (_,e) -> Char(e)) in nt;;


(*****************End of Char parser ******************)

(***************** Nil parser ******************)

let lparen = (char '(');;
let rparen = (char ')');;


(*****************End of Nil parser ******************)



(***************** Sexp  ******************)
  let rec nt_sexpr s = 
    let nt_general = disj_list [nt_boolean;nt_char;nt_string;nt_number;nt_symbol;nt_Nil;nt_list;nt_dotted_list;
    nt_quote;nt_quasi_quote;nt_unquote;nt_unquote_and_splice] in
    (nt_garbage nt_general ) s
    and nt_Nil str =
    let inside = disj_list[(pack nt_whitespace (fun e -> Nil));nt_line_comment;nt_sexpr_comment] in
    let nt = caten lparen (star inside) in
    let nt = caten nt rparen in 
    let nt = pack nt (fun e -> Nil) in nt str
    and nt_list str = 
      let inside = (star nt_sexpr) in 
      let nt = caten lparen inside in
      let nt = caten nt rparen in 
      let nt = pack nt 
      (fun ((_,e),_)  ->
        List.fold_right (fun first_sexp second_sexp -> Pair(first_sexp,second_sexp)) e Nil) in 
      nt str
    and nt_dotted_list str = 
      let garbage = disj_list[(pack nt_whitespace (fun e -> Nil));nt_line_comment;nt_sexpr_comment] in 
      let dot_spaced = make_paired (star garbage) (plus garbage) dot  in
      let inside = caten (plus nt_sexpr) (caten dot_spaced nt_sexpr) in
      let nt = caten lparen (caten inside rparen) in 
      let nt = pack nt (
        function (_,(e,_)) -> match e with
        |(next_sexp, (_, last_sexp)) -> List.fold_right (
          fun first_sexp second_sexp -> Pair(first_sexp,second_sexp))next_sexp last_sexp)in
      nt str 
    and nt_quote str = 
      let quote = word_ci "'" in
      let nt = caten quote nt_sexpr in
      let nt = pack nt (function (_, e) -> Pair(Symbol("quote"), Pair(e, Nil))) in
      nt str
    and nt_quasi_quote str = 
      let quote = word_ci "`" in
      let nt = caten quote nt_sexpr in
      let nt = pack nt (function (_, e) -> Pair(Symbol("quasiquote"), Pair(e, Nil))) in
      nt str
    and nt_unquote str = 
      let quote = word_ci "," in
      let nt = caten quote nt_sexpr in
      let nt = pack nt (function (_, e) -> Pair(Symbol("unquote"), Pair(e, Nil))) in
      nt str
    and nt_unquote_and_splice str = 
      let quote = word_ci ",@" in
      let nt = caten quote nt_sexpr in
      let nt = pack nt (function (_, e) -> Pair(Symbol("unquote-splicing"), Pair(e, Nil))) in
      nt str
    and nt_sexpr_comment str =
      let prefix = word "#;" in
      let inside = nt_sexpr in
      let nt = caten prefix inside in
      let nt = pack nt (fun e -> Nil) in
      nt str
    and nt_garbage str = 
      (* let grabage = disj_list[(pack nt_whitespace (fun e -> Nil));nt_line_comment;nt_sexp_comment] *)
      let garbage = disj_list[(pack nt_whitespace (fun e -> Nil));nt_line_comment;nt_sexpr_comment] in 
      let garbage nt = make_paired (star garbage) (star garbage) nt in 
      garbage str;;
(*****************  end of Sexp  ******************)
(* --- end of parsers --- *)


let read_sexprs string =
    let (list_of_sexp, rest) = ((star nt_sexpr) (string_to_list string)) in
    list_of_sexp ;;


end;; (* struct Reader *)
open Reader;;
(* test zone *)

(* test zone *)
