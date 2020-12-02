#use "reader.ml";;

open PC
open Reader

type constant =
  | Sexpr of sexpr
  | Void

type expr =
  | Const of constant
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list);;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const(Sexpr s1), Const(Sexpr s2) -> sexpr_eq s1 s2
  | Var(v1), Var(v2) -> String.equal v1 v2
  | If(t1, th1, el1), If(t2, th2, el2) -> (expr_eq t1 t2) &&
                                            (expr_eq th1 th2) &&
                                              (expr_eq el1 el2)
  | (Seq(l1), Seq(l2)
    | Or(l1), Or(l2)) -> List.for_all2 expr_eq l1 l2
  | (Set(var1, val1), Set(var2, val2)
    | Def(var1, val1), Def(var2, val2)) -> (expr_eq var1 var2) &&
                                             (expr_eq val1 val2)
  | LambdaSimple(vars1, body1), LambdaSimple(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr_eq body1 body2)
  | LambdaOpt(vars1, var1, body1), LambdaOpt(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr_eq body1 body2)
  | Applic(e1, args1), Applic(e2, args2) ->
     (expr_eq e1 e2) &&
       (List.for_all2 expr_eq args1 args2)
  | _ -> false;;


exception X_syntax_error;;

module type TAG_PARSER = sig
  val tag_parse_expressions : sexpr list -> expr list
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "pset!"; "unquote";
   "unquote-splicing"];;

(* work on the tag parser starts here *)

let rec is_proper_list lst =
  match lst with
  | Pair(head, tail) -> is_proper_list tail
  | Nil -> true
  | _ -> false;;

let rec improper_list_last_elem lst =
  match lst with
  | Pair(head, tail) -> improper_list_last_elem tail
  | Symbol(last) -> last;;

let rec improper_to_proper_list lst =
  match lst with
  | Pair(head, tail) -> Pair(head, improper_to_proper_list tail)
  | Symbol(last) -> Nil;;

let sexpr_to_string x =
  match x with
  | Symbol(x) -> x;;

let rec proper_to_string_list lst =
  match lst with
  | Pair(head, tail) -> [sexpr_to_string head] @ proper_to_string_list tail
  | Nil -> [];;

(**************** Tag Parsers ****************)

let rec tag_parse x =
  match x with
  | Bool(x) -> Const(Sexpr(Bool(x)))
  | Nil -> Const(Sexpr(Nil))
  | Number(x) -> Const(Sexpr(Number(x)))
  | Char(x) -> Const(Sexpr(Char(x)))
  | String(x) -> Const(Sexpr(String(x)))
  | Symbol(x) -> (tag_parse_variable x)
  | Pair(Symbol("quote"), Pair(x, Nil)) -> Const(Sexpr(x))
  (*if than else*)
  | Pair(Symbol("if"), Pair(test, Pair(dit, Pair(dif, Nil)))) ->
      If(tag_parse test, tag_parse dit, tag_parse dif)
   (*if than*)
  | Pair(Symbol("if"), Pair(test, Pair(dit, Nil))) ->
      If(tag_parse test, tag_parse dit, Const(Void))
  | Pair(Symbol("begin"), tail) -> tag_parse_explicitSeq tail
  | Pair(Symbol("set!"),Pair(var,Pair(e,Nil))) -> Set(tag_parse var, tag_parse e)
  | Pair(Symbol("define"),Pair(var,Pair(e,Nil))) -> Def(tag_parse var, tag_parse e)
  | Pair(Symbol("lambda"), tail) -> tag_parse_lambda tail
  | Pair(Symbol("or"), sexprs) -> tag_parse_or sexprs
  (* applic *)
  | Pair( proc,listexp) -> Applic(tag_parse proc, tag_parse_applic listexp)
  (*Macro_expansions*)
  (* cond *)
  | Pair(Symbol("cond"), listSexp) -> tag_parse_cond listSexp

and tag_parse_cond x =
 raise X_not_yet_implemented

and tag_parse_applic x =
  match x with
  | Nil -> [Const(Sexpr(Nil))]
  | Pair(head, Nil) -> [tag_parse head]
  | Pair(head,tail) ->  [tag_parse head] @ tag_parse_applic tail

and tag_parse_variable x =
  if (ormap (fun a -> x = a) reserved_word_list)
  then raise X_no_match
  else Var(x)

and tag_parse_explicitSeq x =
  match x with
  | Nil -> Const(Void)
  | Pair(head, Nil) -> tag_parse head
  | Pair(head, tail) -> Seq(List.flatten (List.map (fun expr -> match expr with
                                                                | Seq(arr) -> arr
                                                                | _ -> [expr]) (create_sequence x)))

and create_sequence x =
  match x with
  | Pair(head, Nil) -> [tag_parse head]
  | Pair(head, tail) -> [tag_parse head] @ create_sequence tail

and tag_parse_lambda x =
  match x with
  | Pair(args, body) when body <> Nil -> (* assuming body != Nil - no empty implicit sequence allowed*)
      if (is_proper_list args)
      then LambdaSimple((proper_to_string_list args),(tag_parse_explicitSeq body))
      else match args with
      | Symbol(vs) -> LambdaOpt([], vs, tag_parse_explicitSeq body)
      | _ -> LambdaOpt(proper_to_string_list (improper_to_proper_list args), improper_list_last_elem args, tag_parse_explicitSeq body)(* improper list *)

and tag_parse_or x =
  match x with
  | Nil -> Const(Sexpr(Bool(false)))
  | Pair(e, Nil) -> Or([tag_parse e])
  | Pair(e, es) -> Or([tag_parse e] @ or_args es)

and or_args x =
  match x with
  | Pair(e, Nil) -> [tag_parse e]
  | Pair(e, es) -> [tag_parse e] @ or_args es
;;
let tag_parse_expressions sexpr =
  List.map tag_parse sexpr;;

end;; (* struct Tag_Parser *)

(*Function for testing*)
open Tag_Parser;;
let test_tag_parse str =
  tag_parse_expressions (read_sexprs str);;