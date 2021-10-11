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


type arith_pred_oper = Eq | Le

type pure_pred = Arith of arith_pred_oper * logical_exp * logical_exp
              | And of pure_pred * pure_pred
              | Prop of logical_fun * (logical_exp list)
              | Neg of pure_pred
              | True | False



type pred_normal_form = {
  pure: ( pure_pred) list; 
  (* disjunctive normal form of pure formulas *)
  spec: fun_signature list;
} 
and fun_signature = {
  fname: program_var;
  pnames: logical_fun list; (* can only occurs on top level *)
  fvar: program_var list;
  fpre: pred_normal_form;
  fpost: program_var * pred_normal_form;
}
(* basic term *)


let rec thin_pred = function
| Arith (oper, t1, t2) -> Arith (oper, t1, t2)
| And (p1, True) -> thin_pred p1
| And (True, p2) -> thin_pred p2
| And (_, False) -> False
| And (False, _) -> False
(* | Or (p1, False) -> thin_pred p1
| Or (False, p2) -> thin_pred p2
| Or (p1, True) -> thin_pred p1
| Or (True, p2) -> thin_pred p2 *)
| And (p1, p2) -> And (thin_pred p1, thin_pred p2)
(* | Or (p1, p2) -> Or (thin_pred p1, thin_pred p2) *)
| Neg p1 -> Neg (thin_pred p1)
| Prop (p, xs) -> Prop (p, xs)
| True -> True
| False -> False

(*******************************
********************************
          Searilizer
********************************
********************************)

let rec logical_exp_to_string = function
| Pvar v -> v
| Lvar v -> v
| Fun (v, vs) -> String.concat "" ([v;"("] @ [(String.concat "," (List.map logical_exp_to_string vs))] @ [")"] )
| Const (Int i) -> string_of_int i
| Op (oper, t1, t2) ->
    let op_str = match oper with | Plus -> "+" | Minus -> "-" | Mult -> "*" in
    String.concat "" ["("; logical_exp_to_string t1; op_str; logical_exp_to_string t2; ")"]


let rec pure_pred_to_string p = 
  let p = thin_pred p in
match p with
| Arith (oper, t1, t2) ->
  let op_str = match oper with | Eq -> "=" | Le -> "<=" in
    String.concat "" [" "; logical_exp_to_string t1; op_str; logical_exp_to_string t2; " "]
| And (p1, p2) ->
    String.concat "" ["("; pure_pred_to_string p1 ; " /\\ "; pure_pred_to_string p2; ")"]
(* | Or (p1, p2) ->
  String.concat "" ["(" ; pure_pred_to_string p1 ; " \\/ "; pure_pred_to_string p2; ")"] *)
| Neg p1 ->
  String.concat "" ["~"; pure_pred_to_string p1]
| Prop (p, xs) ->
    String.concat "" ([p; "("] @ List.map logical_exp_to_string xs @ [")" ])
| True -> " true "
| False -> " false "

let pure_preds_to_string ps =
  String.concat " | " (List.map pure_pred_to_string ps)

let rec string_of_pred_normal_form {pure; spec} = 
  (String.concat " \\/ " (List.map (pure_pred_to_string)
    pure) ) ^ 
    if List.length spec = 0 then "" else
    (" with \n\t" ^
      (String.concat "\n\t" (List.map string_of_fun_spec spec)))

and string_of_fun_spec {fname; fvar; pnames; fpre; fpost} = 
    fname ^ "(" ^ String.concat "," fvar ^ ")"
    ^ "[" ^ String.concat "," pnames ^ "]" ^ "|= {" ^ 
      string_of_pred_normal_form fpre ^ "} *->:" ^ fst fpost ^ " {"
      ^ string_of_pred_normal_form (snd fpost) ^"}"


(*******************************
********************************
    Substitution Function
********************************
********************************)


let rec subst_logical_exp a b =
  let subst_str m = if String.equal m a then b else m in function
| Pvar v -> Pvar (subst_str v)
| Lvar v -> Lvar v
| Fun (v, vs) -> Fun (subst_str v, List.map (subst_logical_exp a b) vs)
| Const (Int i) -> Const (Int i)
| Op (oper, t1, t2) -> Op (oper, subst_logical_exp a b t1, subst_logical_exp a b t2)


let rec subst_pure_pred a b = function
| Arith (oper, t1, t2) -> Arith (oper, (subst_logical_exp a b t1), (subst_logical_exp a b t2))
| And (p1, p2) -> And ((subst_pure_pred a b p1), (subst_pure_pred a b p2))
(* | Or (p1, p2) -> Or ((subst_pure_pred a b p1), (subst_pure_pred a b p2)) *)
| Neg p1 -> Neg (subst_pure_pred a b p1)
| Prop (p, xs) -> Prop (p, List.map (subst_logical_exp a b) xs)
| True -> True
| False -> False

let subst_pure_pred_list a b preds =
  List.map (subst_pure_pred a b) preds 

(* replace ax in p by bx *)
let rec subst_pure_preds ax bx p =
  match ax, bx with
  | a::ax', b::bx' -> 
      let p' = subst_pure_pred a b p in
      subst_pure_preds ax' bx' p'
  | [], [] -> p
  | _ -> failwith "Unmatched argument list in substitution"

let subst_pure_preds_list a b preds =
  List.map (subst_pure_preds a b) preds 

let rec subst_pred_normal_form a b pnf = {
  pure= List.map (fun pred -> subst_pure_pred a b pred) pnf.pure;
  (* disjunctive normal form of pure formulas, every clause comes
     with a list of existential binders of logical variables *)
  spec= List.map (subst_fun_signature a b) pnf.spec
 } 

and subst_fun_signature a b fsig = 
  { fsig with (* TODO: subst pnames? *)
              fpre= subst_pred_normal_form a b fsig.fpre;
              fpost= (* TODO: alpha renaming *)
                (match fsig.fpost with
                anchor, fpost -> (if String.equal a anchor then b else anchor), 
                                  subst_pred_normal_form a b fpost );
              fname = if String.equal a fsig.fname then b else a }


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


    

(*******************************
********************************
    Free Variables
********************************
********************************)

module VarSet = Set.Make(String)

let rec fvars_of_expr e fvars : VarSet.t = 
  let module VS = VarSet in match e with
  | Pvar v -> VS.add v fvars
  | Lvar v -> VS.add v fvars
  | Fun (_, vs) -> 
      (* let fvars1 = VS.add v fvars in *)
      List.fold_right (fvars_of_expr) vs fvars
  | Const _ -> fvars
  | Op (_, t1, t2) -> 
      List.fold_right (fvars_of_expr) [t1; t2] fvars


let rec fvars_of_pure p fvars : VarSet.t = 
  let module VS = VarSet in match p with
  | Arith (_, t1, t2) ->
      List.fold_right (fvars_of_expr) [t1; t2] fvars
  | And (p1, p2) ->
      List.fold_right (fvars_of_pure) [p1; p2] fvars
  (* | Or (p1, p2) ->
      List.fold_right (fvars_of_pure) [p1; p2] fvars *)
  | Neg p -> fvars_of_pure p fvars
  | True | False -> fvars
  | Prop (_, vs) -> 
      List.fold_right (fvars_of_expr) vs fvars
    (* TODO binding relation? *)


let rec fname_of_expr e fvars : VarSet.t = 
  let module VS = VarSet in match e with
  | Fun (v, _) -> VS.add v fvars
  | Op (_, t1, t2) -> 
        List.fold_right (fname_of_expr) [t1; t2] fvars
  | _ -> fvars (* variables *)
  
let fvars_of_pures ps: VarSet.t =
    List.fold_right fvars_of_pure ps VarSet.empty


let rec fname_of_pure p fvars : VarSet.t = 
  let module VS = VarSet in match p with
  | Arith (_, t1, t2) ->
      List.fold_right (fname_of_expr) [t1; t2] fvars
  | And (p1, p2) ->
      List.fold_right (fname_of_pure) [p1; p2] fvars
  (* | Or (p1, p2) ->
      List.fold_right (fname_of_pure) [p1; p2] fvars *)
  | Neg p -> fname_of_pure p fvars
  | Prop (p, vs) ->
      let fvars' = List.fold_right (fname_of_expr) vs fvars in
      VS.add p fvars'
  | True | False -> fvars

let fname_of_pures ps: VarSet.t =
  List.fold_right fname_of_pure ps VarSet.empty

(****************
****************
normalization operations
****************
************)

let preds_and_pred (preds: pure_pred list) pred =
  List.map (fun p -> And (p, pred)) preds



(*   Requires      f(a) |= { true } *->:r { r=fpure(a) }
     Ensures[res]  res=fpure(fpure(a))
*)