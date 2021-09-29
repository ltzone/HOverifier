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

let rec logical_exp_to_string = function
| Pvar v -> v
| LVar v -> v
| Fun (v, vs) -> String.concat "" ([v;"("] @ List.map logical_exp_to_string vs @ [")"] )
| Const (Int i) -> string_of_int i
| Op (oper, t1, t2) ->
    let op_str = match oper with | Plus -> "+" | Minus -> "-" in
    String.concat "" ["("; logical_exp_to_string t1; op_str; logical_exp_to_string t2; ")"]

type arith_pred_oper = Eq | Le

type pure_pred = Arith of arith_pred_oper * logical_exp * logical_exp
               | And of pure_pred * pure_pred
               | Or of pure_pred * pure_pred
               | Neg of pure_pred
               | True | False

let rec pure_pred_to_string = function
| Arith (oper, t1, t2) ->
  let op_str = match oper with | Eq -> "=" | Le -> "<=" in
    String.concat "" [" "; logical_exp_to_string t1; op_str; logical_exp_to_string t2; " "]
| And (p1, p2) ->
    String.concat "" [pure_pred_to_string p1 ; " /\\ "; pure_pred_to_string p2]
| Or (p1, p2) ->
  String.concat "" [pure_pred_to_string p1 ; " \\/ "; pure_pred_to_string p2]
| Neg p1 ->
  String.concat "" ["~"; pure_pred_to_string p1]
| True -> " true "
| False -> " false "


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

let default_pnf = {pure=[([], True)]; spec=[]}

let default_spec = {
  fpre={pure=[([], True)]; spec=[]};
  fpost="res", {pure=[([], True)]; spec=[]};
}


let twice_spec = {
  fpre={pure=[([], True)]; spec=[
    {
      fname="f";
      fvar=["a"];
      fspec={
        fpre={pure=[([], True)]; spec=[]};
        fpost="r", {pure=[([], Arith (Eq, Pvar "r", Fun ("fpure", [Pvar "x"])))]; spec=[]}
      };  
    }

  ]};
  fpost="res", {pure=[([], Arith (Eq, Pvar "res", (Fun ("fpure", [ Fun ("fpure", [Pvar "x"]) ] ))))]; spec=[]};
}


let once_spec = {
  fpre={pure=[([], True)]; spec=[
    {
      fname="f";
      fvar=["a"];
      fspec={
        fpre={pure=[([], True)]; spec=[]};
        fpost="r", {pure=[([], Arith (Eq, Pvar "r", Fun ("fpure", [Pvar "x"])))]; spec=[]}
      };  
    }

  ]};
  fpost="res", {pure=[([], Arith (Eq, Pvar "res", (Fun ("fpure", [Pvar "x"] ))))]; spec=[]};
}

let once_sig = {
  fname="once";
  fvar=["x"];
  fspec=once_spec
}

(*   Requires      f(a) |= { true } *->:r { r=fpure(a) }
     Ensures[res]  res=fpure(fpure(a))
*)