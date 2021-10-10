(* logical expressions *)

type logical_var = string

type program_var = string

type logical_fun = string

type constant = Int of int

type bin_operator = Plus | Minus | Mult

type logical_exp = Pvar of program_var 
                 | Lvar of logical_var 
                 | Fun of logical_fun * (logical_exp list)
                 | Const of constant
                 | Op of bin_operator * logical_exp * logical_exp

let rec logical_exp_to_string = function
| Pvar v -> v
| Lvar v -> v
| Fun (v, vs) -> String.concat "" ([v;"("] @ [(String.concat "," (List.map logical_exp_to_string vs))] @ [")"] )
| Const (Int i) -> string_of_int i
| Op (oper, t1, t2) ->
    let op_str = match oper with | Plus -> "+" | Minus -> "-" | Mult -> "*" in
    String.concat "" ["("; logical_exp_to_string t1; op_str; logical_exp_to_string t2; ")"]

let rec subst_logical_exp a b =
  let subst_str m = if String.equal m a then b else m in function
| Pvar v -> Pvar (subst_str v)
| Lvar v -> Lvar v
| Fun (v, vs) -> Fun (subst_str v, List.map (subst_logical_exp a b) vs)
| Const (Int i) -> Const (Int i)
| Op (oper, t1, t2) -> Op (oper, subst_logical_exp a b t1, subst_logical_exp a b t2)


type arith_pred_oper = Eq | Le

type pure_pred = Arith of arith_pred_oper * logical_exp * logical_exp
               | And of pure_pred * pure_pred
               | Or of pure_pred * pure_pred
               | Neg of pure_pred
               | True | False

let rec thin_pred = function
| Arith (oper, t1, t2) -> Arith (oper, t1, t2)
| And (p1, True) -> thin_pred p1
| And (True, p2) -> thin_pred p2
| And (_, False) -> False
| And (False, _) -> False
| Or (p1, False) -> thin_pred p1
| Or (False, p2) -> thin_pred p2
| Or (p1, True) -> thin_pred p1
| Or (True, p2) -> thin_pred p2
| And (p1, p2) -> And (thin_pred p1, thin_pred p2)
| Or (p1, p2) -> Or (thin_pred p1, thin_pred p2)
| Neg p1 -> Neg (thin_pred p1)
| True -> True
| False -> False

let rec pure_pred_to_string = function
| Arith (oper, t1, t2) ->
  let op_str = match oper with | Eq -> "=" | Le -> "<=" in
    String.concat "" [" "; logical_exp_to_string t1; op_str; logical_exp_to_string t2; " "]
(* | And (p1, True) -> pure_pred_to_string p1
| And (True, p2) -> pure_pred_to_string p2
| And (_, False) -> " false "
| And (False, _) -> " false "
| Or (p1, False) -> pure_pred_to_string p1
| Or (False, p2) -> pure_pred_to_string p2
| Or (p1, True) -> pure_pred_to_string p1
| Or (True, p2) -> pure_pred_to_string p2 *)
| And (p1, p2) ->
    String.concat "" ["("; pure_pred_to_string p1 ; " /\\ "; pure_pred_to_string p2; ")"]
| Or (p1, p2) ->
  String.concat "" ["(" ; pure_pred_to_string p1 ; " \\/ "; pure_pred_to_string p2; ")"]
| Neg p1 ->
  String.concat "" ["~"; pure_pred_to_string p1]
| True -> " true "
| False -> " false "

let rec subst_pure_pred a b = function
| Arith (oper, t1, t2) -> Arith (oper, (subst_logical_exp a b t1), (subst_logical_exp a b t2))
| And (p1, p2) -> And ((subst_pure_pred a b p1), (subst_pure_pred a b p2))
| Or (p1, p2) -> Or ((subst_pure_pred a b p1), (subst_pure_pred a b p2))
| Neg p1 -> Neg (subst_pure_pred a b p1)
| True -> True
| False -> False



(* replace ax in p by bx *)
let rec subst_pure_preds ax bx p =
  match ax, bx with
  | a::ax', b::bx' -> 
      let p' = subst_pure_pred a b p in
      subst_pure_preds ax' bx' p'
  | [], [] -> p
  | _ -> failwith "Unmatched argument list in substitution"



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

let rec subst_pred_normal_form a b pnf = {
  pure= List.map (fun (vs, pred) -> vs, subst_pure_pred a b pred) pnf.pure;
  (* disjunctive normal form of pure formulas, every clause comes
     with a list of existential binders of logical variables *)
  spec= List.map (subst_fun_signature a b) pnf.spec
 } 

and subst_fun_signature a b fsig = 
  { fsig with fspec = subst_spec a b fsig.fspec;
              fname = if String.equal a fsig.fname then b else a }

and subst_spec a b (fun_spec:fun_spec) = {
  fpre= subst_pred_normal_form a b fun_spec.fpre;
  fpost= (* TODO: alpha renaming *)
     match fun_spec.fpost with
     anchor, fpost -> (if String.equal a anchor then b else anchor), 
                      subst_pred_normal_form a b fpost 
}

(* replace ax in p by bx *)
let rec subst_fun_signatures ax bx p =
  match ax, bx with
  | a::ax', b::bx' -> 
      let p' = subst_fun_signature a b p in
      subst_fun_signatures ax' bx' p'
  | [], [] -> p
  | _ -> failwith "Unmatched argument list in substitution"

let rec subst_pred_normal_forms ax bx p =
  match ax, bx with
  | a::ax', b::bx' -> 
      let p' = subst_pred_normal_form a b p in
      subst_pred_normal_forms ax' bx' p'
  | [], [] -> p
  | _ -> failwith "Unmatched argument list in substitution"

let rec subst_specs ax bx p =
  match ax, bx with
  | a::ax', b::bx' -> 
      let p' = subst_spec a b p in
      subst_specs ax' bx' p'
  | [], [] -> p
  | _ -> failwith "Unmatched argument list in substitution"
    


let rec string_of_pred_normal_form {pure; spec} = 
    (String.concat " \\/ " (List.map (fun (vs, nf) -> 
      (if List.length vs = 0 then "" else 
        "forall " ^ String.concat " " vs ^ ", ") ^ pure_pred_to_string nf
      ) pure)) ^ 
      if List.length spec = 0 then "" else
      (" with \n\t" ^
        (String.concat "\n\t" (List.map string_of_fun_spec spec)))

and string_of_fun_spec {fname; fvar; fspec} = 
      fname ^ "(" ^ String.concat "," fvar ^ ") |= {" ^ 
        string_of_pred_normal_form fspec.fpre ^ "} *->:" ^ fst fspec.fpost ^ " {"
        ^ string_of_pred_normal_form (snd fspec.fpost) ^"}"


type environ = fun_spec list

let default_pnf = {pure=[([], True)]; spec=[]}

let default_spec = {
  fpre={pure=[([], True)]; spec=[]};
  fpost="res", {pure=[([], True)]; spec=[]};
}

(*   Requires      f(a) |= { true } *->:r { r=fpure(a) }
     Ensures[res]  res=fpure(fpure(a))
*)