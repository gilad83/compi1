
#use "pc.ml";;
open PC;;
#use "helperFunctions.ml";;

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
  (* Atomic Parsers *)
  val comma : char list -> char * char list
  val colon : char list -> char * char list
  val dot : char list -> char * char list
  val nt_no_new_line : char list -> char * char list
  val semi_colon : char list -> char * char list
  (*Stared Atomic Parsers *)
  val nt_star_whitespaces : char list -> char list * char list
  (* complex Parcsers *)
  val make_paired : ('a -> 'b * 'c) -> ('d -> 'e * 'f) -> ('c -> 'g * 'd) -> 'a -> 'g * 'f 
  val make_spaced : (char list -> 'a * char list) -> char list -> 'a * char list
  val nt_line_comment : char list -> (char * char list) * char list
  val nt_boolean : char list -> sexpr * char list
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
let nt_no_new_line = const (fun ch -> ch != '\n');;
let nt_only_space = (char ' ' );;


(*Stared Atomic Parsers *)
let nt_star_whitespaces = star nt_whitespace;;
let nt_star_only_space = star (char ' ' );;


(* complex Parcsers *)
let make_paired nt_left nt_right nt =
  let nt = caten nt_left nt in
  let nt = pack nt (function (_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt (function (e, _) -> e) in
  nt;;
let make_spaced nt =
  make_paired nt_star_whitespaces nt_star_whitespaces nt;;

let nt_line_comment = 
  (caten semi_colon (star nt_no_new_line));;

let nt_boolean  = 
  let bool_tok = make_spaced (disj ( word_ci "#f")  (word_ci "#t"))  in
  pack bool_tok (fun (bool_t) -> Bool (bool_of_string (bool_t)));;  



(* let nt_symbol = 
  let symbol_tok = caten nt_star_whitespaces ( range_ci_lowercase_ascii 'a' 'z' ) in
  pack symbol_tok = (fun (symbol_tok) -> Symbol(symbol_tok));;  *)
(* --- end of parsers --- *)



let read_sexprs string = raise X_not_yet_implemented;;
end;; (* struct Reader *)



open Reader;;

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

  
let nt_number = disj nt_float (disj nt_fraction nt_integer);;



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
  let pc = star nt_string_char in
  pack pc (fun arr -> list_to_string arr);;

(****************** End of string parser ******************)



(****************** Quote parser ******************)
let nt_quoted = caten (const (fun ch -> ch = ''')) nt_sexpr;;

let nt_quasiQuoted = caten (const (fun ch -> ch = '`')) nt_sexpr;;

let nt_unquoted = caten (const (fun ch -> ch = ',')) nt_sexpr;;

let nt_unquotedAndSpliced = caten (word_ci ",@") nt_sexpr;;
(****************** End of Quote parser ******************)