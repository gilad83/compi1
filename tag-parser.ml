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
exception X_syntax_reverse;;
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
  | Symbol(last) -> last
  | _ -> raise X_syntax_error;;

let rec improper_to_proper_list lst =
  match lst with
  | Pair(head, tail) -> Pair(head, improper_to_proper_list tail)
  | Symbol(last) -> Nil
  | _ -> raise X_syntax_error;;

let sexpr_to_string x =
  match x with
  | Symbol(x) -> x
  | _ -> raise X_syntax_error;;

let rec is_contains a lst =
  match lst with
  | [] -> false
  | head :: tail -> a = head || is_contains a tail;;

let rec is_unique_list lst =
  match lst with
  | [] -> true
  | head :: tail -> not (is_contains head tail) && is_unique_list tail;;

let rec prepare_exp_gen_sym list_of_set_vars str  =
  match list_of_set_vars with
  | [] -> str
  | head :: tail ->
    (match head with
    | Pair(var, Pair(Symbol(val_name), Nil)) -> prepare_exp_gen_sym tail str ^ val_name
    | _ -> prepare_exp_gen_sym tail str);;

let is_reserved_word word =
  (ormap (fun a -> word = a) reserved_word_list);;

let is_contains_reserved_word lst =
  ormap (fun a -> a) (List.map is_reserved_word lst);;

let rec to_list x =
  match x with
  | Nil -> []
  | Pair(e, Nil) -> [sexpr_to_string e]
  | Pair(e, Pair(d, ds)) -> [sexpr_to_string e] @ to_list (Pair(d, ds))
  | Pair(e, es) -> [sexpr_to_string e ; sexpr_to_string es]
  | _ -> [];;

let rec get_let_var_names x =
  match x with
  | Pair(Pair(var, sexpr), Nil) -> Pair(var, Nil)
  | Pair(Pair(var, sexpr), nextVar) -> Pair(var, get_let_var_names nextVar)
  | _ -> raise X_syntax_error;;

let rec get_let_var_sexprs x =
  match x with
  | Pair(Pair(var, Pair(sexpr, Nil)), Nil) -> Pair(sexpr, Nil)
  | Pair(Pair(var, Pair(sexpr, Nil)), nextVar) -> Pair(sexpr, get_let_var_sexprs nextVar)
  | _ -> raise X_syntax_error;;

let rec create_letrec_body x =
  match x with
  | Pair(Pair(Pair(v, sexpr), Nil), body) -> Pair(Pair(Symbol("set!"), Pair(v,sexpr)), Pair(Pair(Symbol("let"), Pair(Nil, body)), Nil))
  | Pair(Pair(Pair(v, sexpr), vs), body) -> Pair(Pair(Symbol("set!"), Pair(v,sexpr)), create_letrec_body (Pair(vs, body)))
  | _ -> raise X_syntax_error;;

  let rec sexprs_to_list_of_set_vars sexprs =
    match sexprs with
  | Pair(set,Nil) -> [set]
  | Pair(set,next_set) -> (sexprs_to_list_of_set_vars next_set) @ [set]
  | _ -> raise X_syntax_error;;

let singel_assign_of_let head num_of_sets gen_sym =
  match head with
  |Pair(var_name,Pair(exp_to_set,rest)) -> Pair(Symbol(gen_sym ^ string_of_int num_of_sets),
  Pair(exp_to_set,Nil))
  |Pair(var_name,exp_to_set) -> Pair(Symbol(gen_sym ^ string_of_int num_of_sets),
  Pair(exp_to_set,Nil))
  |_ -> raise X_syntax_error;;

let rec list_to_let_assignments list_of_set_vars num_of_sets gen_sym pairs=
  match list_of_set_vars with
  | [] -> pairs
  | head::tail -> list_to_let_assignments
    tail (num_of_sets + 1) gen_sym (Pair((singel_assign_of_let head num_of_sets gen_sym),pairs));;

let singel_pair_of_set set_var val_name =
  match set_var with
  | Pair(var_name,Pair(exp_to_set,rest))
  -> Pair(Symbol("set!"),Pair(var_name,(Pair(val_name,Nil))))

  |Pair(var_name,exp_to_set)
  -> Pair(Symbol("set!"),Pair(var_name,(Pair(val_name,Nil))))

  |_ -> raise X_syntax_error;;

let rec list_to_set_pairs list_of_set_vars pair_of_let_assignments pairs =
  match list_of_set_vars,pair_of_let_assignments with
  | [], _ -> pairs
  | head::tail, Pair(Pair(first_val,Pair(sec_val,Nil)),rest) -> list_to_set_pairs
    tail rest (Pair((singel_pair_of_set head first_val),pairs))
  |_,_ -> raise X_syntax_error;;

let rec reverse_pair pair_of_let_assignments pairs =
  match  pair_of_let_assignments with
  |Pair(Pair (c,Pair (d, Nil)), Nil) -> Pair (c,Pair (d, Nil))
  |Pair(Pair(first_val,Pair(sec_val,Nil)),rest)-> Pair(reverse_pair rest pairs,Pair (Pair(first_val,Pair(sec_val,Nil)),Nil))
  | _ -> raise X_syntax_reverse;;
(*
  Pair (Pair (Symbol "c", Pair (Symbol "d", Nil)), Pair (Symbol "a", Pair (Symbol "b", Nil)))
  Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Pair (Pair (Symbol "c", Pair (Symbol "d", Nil)), Nil))
*)
(**************** Macro Expensions ****************)
let expand_and sexprs =
  match sexprs with
  | Pair(e, Nil) -> e
  | Pair(e, es) -> Pair(Symbol("if"), Pair(e, Pair(Pair(Symbol("and"), es), Pair(Bool(false), Nil))))
  | _ -> raise X_syntax_error;;

let expand_let sexprs =
  (*convert to Pair(lambda, let args)*)
  match sexprs with
  | Pair(vars, Nil) -> raise X_syntax_error (*no body for implicit sequence*)
  | Pair(Nil, body) -> Pair(Pair(Symbol("lambda"), Pair(Nil, body)), Nil) (*of type (let () body)*)
  | Pair(vars, body) -> Pair(Pair(Symbol("lambda"), Pair(get_let_var_names vars, body)), get_let_var_sexprs vars)
  | _ -> raise X_syntax_error;;

let expand_letrec sexprs =
  match sexprs with
  | Pair(vars, body) -> Pair(Symbol("let"), Pair(vars, create_letrec_body sexprs))
  | _ -> raise X_syntax_error;;

let rec expand_quasiquote sexprs =
  match sexprs with
  | Pair(Symbol("unquote"), Pair(sexpr, Nil)) -> sexpr
  | Pair(Symbol("unquote-splicing"), Pair(sexpr, Nil)) -> raise X_syntax_error
  | Nil -> Pair(Symbol("quote"), Pair(Nil, Nil))
  | Symbol(sym) -> Pair(Symbol("quote"), Pair(Symbol(sym), Nil))
  | Pair(Pair(Symbol("unquote-splicing"), Pair(sexpr, Nil)), s) -> Pair(Symbol("append"), Pair(sexpr, Pair(expand_quasiquote s, Nil)))
  | Pair(s1, Pair(Symbol("unquote-splicing"), Pair(s2, Nil))) -> Pair(Symbol("cons"), Pair(expand_quasiquote s1, Pair(s2, Nil))) (*added it to make specific test pass = `(,a . ,@b)*)
  | Pair(s1, s2) -> Pair(Symbol("cons"), Pair(expand_quasiquote s1, Pair(expand_quasiquote s2, Nil)))
  | _ -> sexprs;;

  let reverse_list l =
    let rec rev_acc acc = function
      | [] -> acc
      | hd::tl -> rev_acc (hd::acc) tl
    in
    rev_acc [] l ;;

(**************** Tag Parsers ****************)

let rec tag_parse x =
  match x with
  | Bool(x) -> Const(Sexpr(Bool(x)))
  | Nil -> Const(Void)
  | Number(x) -> Const(Sexpr(Number(x)))
  | Char(x) -> Const(Sexpr(Char(x)))
  | String(x) -> Const(Sexpr(String(x)))
  | Symbol(x) -> (tag_parse_variable x)
  | Pair(Symbol("quote"), Pair(x, Nil)) -> Const(Sexpr(x))
  | Pair(Symbol("quasiquote"), Pair(sexprs, Nil)) -> tag_parse_quasiquote sexprs
  (*if than else*)
  | Pair(Symbol("if"), Pair(test, Pair(dit, Pair(dif, Nil)))) ->
      If(tag_parse test, tag_parse dit, tag_parse dif)
   (*if than*)
  | Pair(Symbol("if"), Pair(test, Pair(dit, Nil))) ->
      If(tag_parse test, tag_parse dit, Const(Void))
  | Pair(Symbol("begin"), tail) -> tag_parse_explicitSeq tail
  | Pair(Symbol("set!"),Pair(var,Pair(e,Nil))) -> Set(tag_parse var, tag_parse e)
  (* Mit define *)
  | Pair(Symbol "define", Pair(Pair(var,arglist),exp_plus)) -> expand_mit_define var arglist exp_plus
    (*define*)
  | Pair(Symbol("define"),Pair(var,exp)) -> tag_parse_Def var exp
  | Pair(Symbol("lambda"), tail) -> tag_parse_lambda tail
  | Pair(Symbol("or"), sexprs) -> tag_parse_or sexprs
  | Pair(Symbol("and"), sexprs) -> tag_parse_and sexprs
  (* cond *)
  | Pair(Symbol("cond"), listSexp) -> tag_parse(tag_parse_cond listSexp)
  | Pair(Symbol("let"), sexprs) -> tag_parse_let sexprs
  | Pair(Symbol("let*"), sexprs) -> tag_parse_letstar sexprs
  | Pair(Symbol("letrec"), sexprs) -> tag_parse_letrec sexprs
  | Pair(Symbol("pset!"), sexprs) ->  expand_pset sexprs
      (* applic *)
  | Pair( proc,listexp) -> Applic(tag_parse proc, tag_parse_applic listexp)
  (*Macro_expansions*)

and expand_pset pset_expr =
  let list_of_set_vars = sexprs_to_list_of_set_vars pset_expr in
  let list_of_set_vars_rev = reverse_list list_of_set_vars in
  if (List.length list_of_set_vars == 1) then
  only_one_set pset_expr
  else
  let gen_sym =  prepare_exp_gen_sym list_of_set_vars "exp" in
  let pair_of_let_assignments = list_to_let_assignments list_of_set_vars 0 gen_sym Nil in
  let pair_of_sets = list_to_set_pairs list_of_set_vars_rev pair_of_let_assignments Nil in
  tag_parse (Pair (Symbol "let",
  Pair (pair_of_let_assignments,
  Pair (Pair(Symbol("begin"),pair_of_sets), Nil))))

  (* raise X_syntax_error *)

and only_one_set pset_expr =
  (match pset_expr with
  | Pair(set, Nil) -> tag_parse (Pair(Symbol("set!"), set))
  | _ -> raise X_syntax_error)

and tag_parse_Def var exp =
match exp with
 Nil -> Def(tag_parse var,Const(Void))
| Pair(concrete_exp,Nil) -> Def(tag_parse var, tag_parse concrete_exp)
| _ -> raise X_syntax_error

and expand_mit_define var arglist exp_plus =
tag_parse (Pair(Symbol "define", Pair(var, Pair(Pair(Symbol("lambda"), Pair( arglist, exp_plus)),Nil))))



and tag_parse_cond x =
 match x with
  (* (else form) *)
  Pair(Pair(Symbol("else"),dit),_) -> Pair(Symbol("begin"),dit)
  (* The arrow-form no cont *)
  |Pair(Pair(test, Pair(Symbol "=>", Pair(dit_apply, Nil))),Nil) ->
    Pair(Symbol "let", Pair(Pair(Pair(Symbol "value", Pair(test, Nil)),
    Pair(Pair(Symbol "f", Pair(Pair(Symbol "lambda", Pair(Nil, Pair(dit_apply, Nil))), Nil)), Nil)),
    Pair(Pair(Symbol "if", Pair(Symbol "value", Pair(Pair(Pair(Symbol "f", Nil), Pair(Symbol "value", Nil)), Nil))), Nil)))



(* The arrow-form with cont *)
|Pair(Pair(test, Pair(Symbol "=>", Pair(dit_apply, Nil))),cont) -> Pair(Symbol "let", Pair(Pair(Pair(Symbol "value", Pair(Pair(test, Nil), Nil)),
 Pair(Pair(Symbol "f", Pair(Pair(Symbol "lambda", Pair(Nil, Pair(Pair(dit_apply, Nil), Nil))), Nil)),
  Pair(Pair(Symbol "rest", Pair(Pair(Symbol "lambda", Pair(Nil, Pair(Pair((tag_parse_cond cont), Nil), Nil))), Nil)), Nil))),
   Pair(Pair(Symbol "if", Pair(Symbol "value", Pair(Pair(Pair(Symbol "f", Nil), Pair(Symbol "value", Nil)), Pair(Pair(Symbol "rest", Nil), Nil)))), Nil)))



  (* (common form) *)
  |Pair(Pair(test,dit),Nil) -> (Pair(Symbol"if",Pair(test, Pair(Pair(Symbol("begin"),dit), Nil))))
  |Pair(Pair(test,dit),next_rib) -> let next_rib = tag_parse_cond next_rib in
  (Pair(Symbol"if",Pair(test, Pair(Pair(Symbol("begin"),dit), Pair(next_rib,Nil)))))
  |_ -> raise X_syntax_error

and tag_parse_applic x =
  match x with
  | Nil -> []
  | Pair(head, Nil) -> [tag_parse head]
  | Pair(head,tail) ->  [tag_parse head] @ tag_parse_applic tail
  | _ -> raise X_syntax_error

and tag_parse_variable x =
  if (is_reserved_word x)
  then raise X_no_match
  else Var(x)

and tag_parse_explicitSeq x =
  match x with
  | Nil -> Const(Void)
  | Pair(head, Nil) -> tag_parse head
  | Pair(head, tail) -> Seq(List.flatten (List.map (fun expr -> match expr with
                                                                | Seq(arr) -> arr
                                                                | _ -> [expr]) (create_sequence x)))
  | _ -> raise X_syntax_error

and create_sequence x =
  match x with
  | Pair(head, Nil) -> [tag_parse head]
  | Pair(head, tail) -> [tag_parse head] @ create_sequence tail
  | _ -> raise X_syntax_error

and tag_parse_lambda x =
  match x with
  | Pair(args, body) when body <> Nil -> (* assuming body != Nil - no empty implicit sequence allowed*)
    (match args with
    | Symbol(vs) -> if is_reserved_word vs
                    then raise X_syntax_error
                    else LambdaOpt([], vs, tag_parse_explicitSeq body)
    | _ ->
      if (is_unique_list (to_list args)) && not (is_contains_reserved_word (to_list args))
      then if (is_proper_list args)
          then LambdaSimple((to_list args),(tag_parse_explicitSeq body))
          else LambdaOpt(to_list (improper_to_proper_list args), improper_list_last_elem args, tag_parse_explicitSeq body)
      else raise X_syntax_error)
  | _ -> raise X_syntax_error

and tag_parse_or x =
  match x with
  | Nil -> Const(Sexpr(Bool(false)))
  | Pair(e, Nil) -> Or([tag_parse e])
  | Pair(e, es) -> Or([tag_parse e] @ or_args es)
  | _ -> raise X_syntax_error

and or_args x =
  match x with
  | Pair(e, Nil) -> [tag_parse e]
  | Pair(e, es) -> [tag_parse e] @ or_args es
  | _ -> raise X_syntax_error

and tag_parse_and x =
  match x with
  | Nil -> Const(Sexpr(Bool(true)))
  | _ -> tag_parse (expand_and x)

and tag_parse_let x =
  match x with
  | Pair(vars, body) -> tag_parse (expand_let x)
  | _ -> raise X_syntax_error

and tag_parse_letstar x =
  match x with
  | Pair(Nil, body) -> tag_parse_let x
  | Pair(Pair(Pair(var, sexpr), Nil), body) -> tag_parse_let x
  | Pair(Pair(Pair(var, sexpr), vars), body) -> tag_parse (Pair(Symbol("let"), Pair(Pair(Pair(var, sexpr), Nil), Pair(Pair(Symbol("let*"), Pair(vars, body)), Nil))))
  | _ -> raise X_syntax_error

and tag_parse_letrec x =
  match x with
  | Pair(vars, body) -> tag_parse (expand_letrec x)
  | _ -> raise X_syntax_error

and tag_parse_quasiquote x =
  tag_parse (expand_quasiquote x)
  ;;


let tag_parse_expressions sexpr =
  List.map tag_parse sexpr;;

end;; (* struct Tag_Parser *)

(*Function for testing*)
open Tag_Parser;;
let test_tag_parse str =
  tag_parse_expressions (read_sexprs str);;


