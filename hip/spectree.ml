(* logical expressions *)

type logical_var = string

type program_var = string

type logical_fun = string

type constant = Int of int

type bin_operator = Plus | Minus 

type logical_exp = Pvar of program_var 
                 | LVar of logical_var 
                 | Fun of logical_fun * (logical_exp list)
                 | Const of constant
                 | Op of bin_operator * logical_exp * logical_exp

type arith_pred_oper = Eq | Le

type pure_pred = Arith of arith_pred_oper * logical_exp * logical_exp
               | And of pure_pred * pure_pred
               | Or of pure_pred * pure_pred
               | Neg of pure_pred
               | True | False


type pred_normal_form = {
  pure: (logical_var list * pure_pred) list; 
  (* disjunctive normal form of pure formulas, every clause comes
     with a list of existential binders of logical variables *)
  spec: fun_signature list;
} 
and fun_spec = {
  fpre: pred_normal_form;
  fpost: program_var * pred_normal_form;
} 
and fun_signature = {
  fname: program_var;
  fvar: program_var list;
  fspec: fun_spec;  
}
(* basic term *)

type environ = fun_spec list

let default_spec = {
  fpre={pure=[([], True)]; spec=[]};
  fpost="res", {pure=[([], True)]; spec=[]};
}