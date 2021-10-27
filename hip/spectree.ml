(* logical expressions *)




type logical_var = string

type program_var = string


type constant = Int of int 
(* | Bool of bool *)

type bin_operator = Plus | Minus | Mult

type logical_exp = Pvar of program_var 
| Lvar of logical_var 
(* | Fun of logical_fun * (logical_exp list) *)
| Const of constant
| Op of bin_operator * logical_exp * logical_exp


type exp_type = Int 
(* | Bool *)

type fun_id = string

type logical_fun = fun_id * (logical_var * exp_type) list

type arith_pred_oper = Eq | Le

type pure_pred = Arith of arith_pred_oper * logical_exp * logical_exp
              | And of pure_pred * pure_pred
              | Prop of fun_id * (logical_exp list)
              | Neg of pure_pred
              | True | False

type pred = pure_pred


type pred_normal_form = {
  pure: pred list; 
  (* disjunctive normal form of pure formulas *)
  spec: fun_signature list;
} 
and fun_signature = {
  fname: program_var;
  pnames: fun_id list; (* can only occurs on top level *)
  fvar: program_var list;
  fpre: pred_normal_form;
  fpost: program_var * pred_normal_form;
}
(* basic term *)


type logical_proposition = {
  pname: fun_id;
  pargs: (logical_var * exp_type) list;
  pbody: pred list; (* disjunctive normal form *)
}

type spec_res = 
| Fail
| Success
| Inst of ((fun_id * logical_proposition) list) list

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
(* | Fun (v, vs) -> String.concat "" ([v;"("] @ [(String.concat "," (List.map logical_exp_to_string vs))] @ [")"] ) *)
| Const (Int i) -> string_of_int i
(* | Const (Bool i) -> string_of_bool i *)
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
    String.concat "" ([p; "(" ; String.concat "," (List.map logical_exp_to_string xs) ; ")" ])
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

let string_of_ty = function
| Int -> "int"
(* | Bool -> "bool" *)

let logical_proposition_to_string  {pname;  pargs;  pbody;} = 
  String.concat ""
  [pname; "("; String.concat "," (List.map (fun (v,t) -> v ^ ":" ^ string_of_ty t) pargs);
      ") = {"; pure_preds_to_string pbody ;"}" ]


(*******************************
********************************
    Substitution Function
********************************
********************************)


let rec subst_logical_exp a b =
  let subst_str m = if String.equal m a then b else m in function
| Pvar v -> Pvar (subst_str v)
| Lvar v -> Lvar v
(* | Fun (v, vs) -> Fun (subst_str v, List.map (subst_logical_exp a b) vs) *)
| Const x -> Const x
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


let rec fill_logical_exp a (b:logical_exp) = function
| Pvar v -> if String.equal v a then b else Pvar v
| Lvar v -> if String.equal v a then b else Lvar v
(* | Fun (v, vs) -> Fun (subst_str v, List.map (subst_logical_exp a b) vs) *)
| Const (Int i) -> Const (Int i)
| Op (oper, t1, t2) -> Op (oper, fill_logical_exp a b t1, fill_logical_exp a b t2)


let rec fill_pure_pred a b = function
| Arith (oper, t1, t2) -> Arith (oper, (fill_logical_exp a b t1), (fill_logical_exp a b t2))
| And (p1, p2) -> And ((fill_pure_pred a b p1), (fill_pure_pred a b p2))
(* | Or (p1, p2) -> Or ((fill_logical_exp a b p1), (fill_logical_exp a b p2)) *)
| Neg p1 -> Neg (fill_pure_pred a b p1)
| Prop (p, xs) -> Prop (p, List.map (fill_logical_exp a b) xs)
| True -> True
| False -> False

let fill_pure_pred_list a b preds =
  List.map (fill_pure_pred a b) preds 
  
let rec fill_pure_preds ax bx p =
match ax, bx with
| a::ax', b::bx' -> 
    let p' = fill_pure_pred a b p in
    fill_pure_preds ax' bx' p'
| [], [] -> p
| _ -> failwith "Unmatched argument list in filling"

let fill_pure_preds_list ax bx preds =
  List.map (fill_pure_preds ax bx) preds 

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
  (* if String.equal fsig.fname a then fsig else *)
  { fsig with (* TODO: subst pnames? *)
              fpre= subst_pred_normal_form a b fsig.fpre;
              fpost= (* TODO: alpha renaming *)
                (match fsig.fpost with
                anchor, fpost -> (if String.equal a anchor then b else anchor), 
                                  subst_pred_normal_form a b fpost );
              fname = if String.equal a fsig.fname then b else fsig.fname
  }
(* fname works as a binder and therefore needs not substitution *)


let rec subst_fun_signature_name a b fsig = 
  let new_pre = { fsig.fpre with spec = List.map (subst_fun_signature_name a b) fsig.fpre.spec } in
  let new_post = (fst fsig.fpost, { (snd fsig.fpost) with 
  spec = List.map (subst_fun_signature_name a b) ((snd fsig.fpost).spec) }) in
  let new_name = if String.equal a fsig.fname then b else fsig.fname in
  { fsig with fpre = new_pre;
              fpost = new_post;
              fname = new_name }


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
  (* | Fun (_, vs) -> 
      (* let fvars1 = VS.add v fvars in *)
      List.fold_right (fvars_of_expr) vs fvars *)
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
  (* | Fun (v, _) -> VS.add v fvars *)
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



module SMap = Map.Make (struct
type t = string
let compare = compare
end)

module SSet = Set.Make(String)

type env = {
(* module name -> a bunch of function specs *)
specs : fun_signature list SMap.t;

res_index : int ref;

predicates : logical_proposition list;

fname_assignment: (logical_proposition) SMap.t;
(* ~~Keep track of the possible list fname assignments in the context
   If one forward verifiaction process fails,
   the assignment should be removed~~

   The current assignment of fnames, no duplicate
*)

ftype_context: (exp_type list) SMap.t
(* when simple SMT solving fails,
   try to find a proper fname assignment
   
   what about when fname are composed?

   Proposed strategy

   1. same
   2. try fnames in the context (uninterpreted)
   (* do we really need this feature? *)
   3. try concrete abstractions (interpreted)
   
   *)
(* fnames : SSet.t; *)
}


module Env = struct
let empty = {
  specs = SMap.empty;
  res_index = ref 0;
  predicates = [];
  fname_assignment = SMap.empty;
  ftype_context = SMap.empty;
}

let insted_preds env = List.map fst (SMap.bindings env.fname_assignment)

let lookup_ftype env fname =
  match SMap.find_opt fname env.ftype_context with
  | Some v -> fname, v
  | None -> failwith ("Predicate type not found for " ^ fname)

let add_inst env inst_name inst = 
  match SMap.find_opt inst_name env.fname_assignment with
  | Some ass -> failwith (inst_name ^ " is already instantiated as " ^ logical_proposition_to_string ass)
  | None ->
    {
      env with
      fname_assignment= SMap.add inst_name inst env.fname_assignment;
    }
let add_inst_group env (insts: (fun_id * logical_proposition) list) = 
  List.fold_right (fun (inst_name, inst) env -> add_inst env inst_name inst) insts env
  
let print_all_insts env =
  let insts = env.fname_assignment in
  List.iter (fun (name, name_insts) ->
    print_string (name ^ ": ");
    print_endline (logical_proposition_to_string name_insts)
  )  (SMap.bindings insts)


let add_fn fname specs env =
  { env with specs = SMap.add fname specs env.specs; }

let add_spec_to_fn fname spec env = 
  { env with specs = SMap.update fname (function None -> Some [spec]
                              | Some specs -> Some (spec::specs)) env.specs }


let find_spec fname env = SMap.find_opt fname env.specs

let get_fresh_res_name env = env.res_index := !(env.res_index) + 1; "_r" ^ string_of_int !(env.res_index)

let get_top_res_name env = "_r" ^ string_of_int !(env.res_index)

let available_names env = List.map fst (SMap.bindings (env.specs))

end

let rec instantiate_pred fname assign pure_pred : pure_pred list =
  match pure_pred with
  | Arith (op, x1, x2) -> [ Arith (op, x1, x2) ]
  | And (p1, p2) -> 
      let p1_inst = instantiate_pred fname assign p1 in
      let p2_inst = instantiate_pred fname assign p2 in
      List.concat_map (fun v -> List.map (fun x -> And (v, x)) p1_inst) p2_inst 
  | Prop (fn, es) ->
      if String.equal fname fn
        then fill_pure_preds_list (List.map fst (assign.pargs)) es (assign.pbody)
      else  [ Prop (fn, es) ]
  | Neg p -> instantiate_pred fname assign p
  | True  -> [ True ]
  | False -> [ False ]


let instantiate_preds fname assign pure_preds : pure_pred list =
  List.concat_map (instantiate_pred fname assign) pure_preds

let instantiate_pure_preds fname_assignment pure_preds =
  if List.length (fname_assignment) > 0 then
    print_endline (logical_proposition_to_string (snd (List.nth ( fname_assignment) 0)))
  else ();
  
  List.fold_right (fun (fname, (ass:logical_proposition)) pred -> instantiate_preds fname ass pred) fname_assignment pure_preds
