
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
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2);;

<<<<<<< HEAD
(************* our functions *************)
let ascii_0 = 48;;

let nt_whitespaces = star nt_whitespace;;

let digit = range '0' '9';;

let digit_seq = plus digit;;

(* fold_left func [] [0,0,0,1,2,0] *)

let zero = const (fun ch -> ch = '0');;

let zeros = star zero;;

let natural s = 
  let (zrs, rest) = (zeros s) in
  List.fold_left 
    (pack digit (fun ch -> (int_of_char ch) - ascii_0))
    nt_epsilon
    rest;;

(* let integer = caten sign natural_num;; *)

let mantissa = digit_seq;;
(************* end of our functions *************)
=======
module Reader: sig
  val read_sexprs : string -> sexpr list
  val comma : char list -> char * char list
  val colon : char list -> char * char list
  val dot : char list -> char * char list
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | _ -> false;;
>>>>>>> 045032dbb345be52d8a26fd940cf4c50e1dad4ba

module Reader: sig
  val read_sexprs : string -> sexpr list
  val tests : unit -> unit
end
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;

let tests () = 
    assert((PC.test_string digit_seq "022a") = (['0';'2';'2'], "->[a]"));;


(* Atomic Parsers *)
let comma = (char ',');;
let colon = (char ':');;
let dot = (char '.');;

(*Stared Atomic Parsers *)
let nt_star_whitespaces = star nt_whitespace;;

let read_sexprs string = raise X_not_yet_implemented;;
  
end;; (* struct Reader *)
