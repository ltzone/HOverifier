open List
open Spectree
(* open Pretty *)
open Z3
exception TestFailedException of string

let print_length xs = print_endline ("Length is :" ^ string_of_int (List.length xs))

let print_assignment_groups vs =
  print_endline "[[[[[candidate assignments ]]]]]";
  List.iter (fun vs' ->
    List.iter (fun (n, v) -> print_endline (n ^" : " ^ logical_proposition_to_string v)) vs';
    print_endline "----------------------------") vs;

type prop_candidates = (fun_id * logical_proposition) list

let list_diff a b =
  let a_set = VarSet.of_list a in
  let b_set = VarSet.of_list b in
  let a_diff_b = VarSet.diff a_set b_set in
  VarSet.elements a_diff_b

let list_intersect a b =
  let a_set = VarSet.of_list a in
  let b_set = VarSet.of_list b in
  let a_set_b = VarSet.inter a_set b_set in
  VarSet.elements a_set_b

  

let forall_formula_of ctx xs formula =
  let xs = List.map (fun v -> 
      Expr.mk_const_s ctx v (Arithmetic.Integer.mk_sort ctx)) xs in
  if List.length xs = 0 then formula else
  Quantifier.expr_of_quantifier (Quantifier.mk_forall_const ctx xs formula 
                        (Some 1) [] [] None None) 

let exists_formula_of ctx xs formula =
  let xs = List.map (fun v -> 
      Expr.mk_const_s ctx v (Arithmetic.Integer.mk_sort ctx)) xs in
  if List.length xs = 0 then formula else
  Quantifier.expr_of_quantifier (Quantifier.mk_exists_const ctx xs formula 
                        (Some 1) [] [] None None) 

let rec expr_to_expr ctx : logical_exp -> Expr.expr = function
  | Pvar v  -> Arithmetic.Integer.mk_const_s ctx v
  | Lvar v -> Arithmetic.Integer.mk_const_s ctx v
  (* | Fun (f, vs) -> 
      let arg_list = List.init (List.length vs) (fun _ -> (Arithmetic.Integer.mk_sort ctx)) in
      let target_sort = Arithmetic.Integer.mk_sort ctx in
      let fun_decl = FuncDecl.mk_func_decl_s ctx f arg_list target_sort in
      let fun_args = 
        List.map (expr_to_expr ctx) vs  in
      Expr.mk_app ctx fun_decl fun_args *)
  | Const (Int n) -> Arithmetic.Integer.mk_numeral_i ctx n
  | Op (oper , t1, t2) -> 
      let z3_constr = match oper with 
                    | Plus -> Arithmetic.mk_add 
                    | Minus -> Arithmetic.mk_sub
                    | Mult -> Arithmetic.mk_mul in
      let t1_expr = expr_to_expr ctx t1 in
      let t2_expr = expr_to_expr ctx t2 in
      z3_constr ctx [t1_expr ; t2_expr ]

let rec pure_pred_to_expr ctx pred : Expr.expr =
  let thinned_pred = thin_pred pred in
  match thinned_pred with
  | Arith (oper, t1, t2) ->
      let t1_exp = expr_to_expr ctx t1 in
      let t2_exp = expr_to_expr ctx t2 in
      let oper_fun = match oper with
      | Eq -> Boolean.mk_eq
        (* fun ctx e1 e2 -> Boolean.mk_and ctx [Arithmetic.mk_le ctx e1 e2 ; Arithmetic.mk_le ctx e2 e1 ] *)
      | Le -> Arithmetic.mk_le in (* TODO: is this correct? *)
      oper_fun ctx t1_exp t2_exp
  | And (p1, p2) ->
      let p1_exp = pure_pred_to_expr ctx p1 in
      let p2_exp = pure_pred_to_expr ctx p2 in
      Boolean.mk_and ctx [p1_exp; p2_exp]
  (* | Or (p1, p2) -> 
    let p1_exp = pure_pred_to_goal ctx p1 in
    let p2_exp = pure_pred_to_goal ctx p2 in
      Boolean.mk_or ctx [p1_exp; p2_exp] *)
  | Neg p ->
      let p = pure_pred_to_expr ctx p in
      Boolean.mk_not ctx p
  | Prop (p, vs) ->
      let arg_list = List.init (List.length vs) (fun _ -> (Arithmetic.Integer.mk_sort ctx)) in
      let target_sort = Boolean.mk_sort ctx in
      let fun_decl = FuncDecl.mk_func_decl_s ctx p arg_list target_sort in
      let fun_args = 
        List.map (expr_to_expr ctx) vs  in
      Expr.mk_app ctx fun_decl fun_args
  | True -> Boolean.mk_true ctx
  | False -> Boolean.mk_false ctx

let pure_preds_to_expr ctx preds : Expr.expr =
  let fold_or pred expr =
    let pred_expr = pure_pred_to_expr ctx pred in
    Boolean.mk_or ctx [pred_expr; expr] in
  List.fold_right fold_or preds (Boolean.mk_false ctx) 


let solver_wrapper ctx goal : bool =
  (* discharge a goal by solving its negation
     so that uninterpreted functions can be universally quantified on toplevel
  *)
  let solver = (Solver.mk_simple_solver ctx) in
    let f e = (Solver.add solver [ Boolean.mk_not ctx e ]) in
      ignore (List.map f (Goal.get_formulas goal)) ;
    let q = (Solver.check solver []) in
      if q != SATISFIABLE then 
        (Printf.printf "Solver says: %s\n" (Solver.string_of_status q) ;
        (Printf.printf "Entailment success\n") ; true)
      else
        let m = (Solver.get_model solver) in    
        match m with 
    | None -> 
      raise (TestFailedException "")
    | Some (m) -> 
      Printf.printf "Solver says: %s\n" (Solver.string_of_status q) ;
      (Printf.printf "Entailment fail\n") ;
      Printf.printf "Model: \n%s\n" (Model.to_string m);
      let n = (Model.get_const_decls m) in    
        List.iter (fun v -> print_endline (FuncDecl.to_string v)) n ; false

let encode_ty ctx = function
 Int -> Arithmetic.Integer.mk_sort ctx

let encode_arg ctx = function
| (v, Int) -> Arithmetic.Integer.mk_const_s ctx v

let logical_proposition_to_expr ctx fname {pargs; pbody; _} : Expr.expr =
  let arg_list = List.map fst pargs in
  let arg_ty_list = List.map ((encode_ty ctx)) (List.map (snd) pargs) in
  let var_list = List.map (encode_arg ctx) pargs in
  let ex_vars = list_diff (VarSet.elements (fvars_of_pures pbody)) arg_list in
  let fun_decl = FuncDecl.mk_func_decl_s ctx fname arg_ty_list (Boolean.mk_sort ctx) in
  forall_formula_of ctx arg_list 
    (Boolean.mk_eq ctx (FuncDecl.apply fun_decl var_list) 
      (exists_formula_of ctx ex_vars (pure_preds_to_expr ctx pbody)))


(* 
z3 Cannot synthesize a fpure itself!
*)
(* let solver_check_bool' ctx goal =
  let solver = (Solver.mk_simple_solver ctx) in
    let f e = 
      (* (print_endline (Expr.to_string (Boolean.mk_not ctx e)); *)
    Solver.add solver [ e ] in
      ignore (List.map f (Goal.get_formulas goal)) ;
    let q = (Solver.check solver []) in
      if q != SATISFIABLE then false
      else
        let m = (Solver.get_model solver) in    
        match m with 
    | None -> 
      raise (TestFailedException "")
    | Some (m) -> 
      Printf.printf "Solver says: %s\n" (Solver.string_of_status q) ;
      (Printf.printf "Entailment success\n") ;
      Printf.printf "Model: \n%s\n" (Model.to_string m);
      let n = (Model.get_const_decls m) in    
        List.iter (fun v -> print_endline (FuncDecl.to_string v)) n; true *)



let solver_check_bool ctx goal (cand: prop_candidates) =
  let solver = (Solver.mk_simple_solver ctx) in
    let f e = 
      (* (print_endline (Expr.to_string (Boolean.mk_not ctx e)); *)
    Solver.add solver [ Boolean.mk_not ctx e ] in
      ignore (List.map f (Goal.get_formulas goal)) ;
    List.iter (fun (fname, fbody) -> 
      print_endline (Expr.to_string (logical_proposition_to_expr ctx fname fbody));
      Solver.add solver
      [logical_proposition_to_expr ctx fname fbody]) cand;
    let q = (Solver.check solver []) in
      if q != SATISFIABLE then true
      else
        let m = (Solver.get_model solver) in    
        match m with 
    | None -> 
      raise (TestFailedException "")
    | Some (m) -> 
      Printf.printf "Solver says: %s\n" (Solver.string_of_status q) ;
      (Printf.printf "Entailment fail\n") ;
      Printf.printf "Model: \n%s\n" (Model.to_string m);
      let n = (Model.get_const_decls m) in    
        List.iter (fun v -> print_endline (FuncDecl.to_string v)) n; false

let var_quantifier_of_spec (spec: fun_signature) : logical_var list * logical_var list =
  let pre = spec.fpre.pure in
  let post = (snd spec.fpost).pure in
  let fvars = VarSet.of_list spec.fvar in
  let pre_fvs = fvars_of_pures pre in
  let post_fvs = fvars_of_pures post in
  let post_exs = VarSet.diff post_fvs pre_fvs in 
  let forall_vars = VarSet.elements (VarSet.union pre_fvs fvars) in
  let exists_vars = VarSet.elements (VarSet.diff post_exs fvars) in 
  (forall_vars, exists_vars)


let rename_fun_args fun1 fun2 = 
  if (List.length fun2.pnames != 0)
    then failwith (fun2.fname ^ " has abstract predicates not instantiated");
  if (List.length fun2.fvar != List.length fun1.fvar)
    then failwith (fun1.fname ^ " has unmatched arguments with " ^ fun2.fname);
  {
  fun2 with
    fvar = fun1.fvar;
    fpre = subst_pred_normal_forms (fun2.fvar) (fun1.fvar) fun2.fpre;
    fpost = (fst fun1.fpost ,
      let subst_ret = subst_pred_normal_form 
        (fst fun2.fpost) (fst fun1.fpost) (snd fun2.fpost) in
      subst_pred_normal_forms (fun2.fvar) (fun1.fvar) subst_ret)
  }


let make_prop_candidates env (fnames: (fun_id * exp_type list) list) :
  (fun_id * logical_proposition) list list =
  let check_sig_match arg cand_arg = 
    (* TODO: add check type *)
      List.length arg = List.length cand_arg in
  let find_match_prop fname =
    List.filter (fun v -> check_sig_match fname (v.pargs)) env.predicates in
  let res = List.fold_left (fun (res: (fun_id * logical_proposition) list list) (fname, fargs) ->
    let cur_candidates = (List.map (fun v -> (fname, v)) (find_match_prop fargs)) in
      List.concat_map (fun v -> List.map (fun r -> (v::r)) res) cur_candidates
  ) [[]] fnames in res



let check_spec_sub (env:env) (pre: pure_pred list) fun1 fun2 : spec_res = 
(* For client signature GIVEN Z1, pre1 *-> exist x.post1
   and server signature GIVEN Z2, pre2 *-> exist y.post2
   fun1 <: fun2 should be encoded into

   Forall Z1, pre /\ pre2 -> 
    (Exists Z2, pre1 /\
      (exists y. post1 -> exists x. post2))

   To make SMT work, Z2 needs to be instantiated in advance
   so that we can use negation technique to check the globally
   quantified Z1
*)
  print_endline "sleek: checking for subsumption";
  print_endline ("[context] " ^ pure_preds_to_string pre);
  print_endline ("[client] " ^ string_of_fun_spec fun1);
  print_endline ("[server] " ^ string_of_fun_spec fun2);

(* Step 1: translate to Z3 expression *)
  let fun2 = rename_fun_args fun1 fun2 in
  (* fun1 is also associated with pre, so we rename fun2 *)

  let spec1_all, spec1_ex = var_quantifier_of_spec fun1 in
  let spec2_all, spec2_ex = var_quantifier_of_spec fun2 in
  let arg_all = fun1.fvar in
  let spec1_all = list_diff spec1_all arg_all in
  let spec2_all = list_diff spec2_all arg_all in
  let shared_ex = list_intersect spec1_ex spec2_ex in
  let spec1_ex = list_diff spec1_ex shared_ex in
  let spec2_ex = list_diff spec2_ex shared_ex in

  let cfg = [("model", "true"); ("proof", "true")] in
  let ctx = (mk_context cfg) in
  let goal = Goal.mk_goal ctx true true true in 
  let pre_formula = (pure_preds_to_expr ctx pre) in
  let pre1_formula = (pure_preds_to_expr ctx fun1.fpre.pure) in
  let pre2_formula = (pure_preds_to_expr ctx fun2.fpre.pure) in
  let post1_formula = (pure_preds_to_expr ctx (snd fun1.fpost).pure) in
  let post2_formula = (pure_preds_to_expr ctx (snd fun2.fpost).pure) in
  let impl_formula_lhs = Boolean.mk_and ctx [pre_formula; pre2_formula] in
  let impl_formula_rhs = Boolean.mk_and ctx [
      pre1_formula;
      forall_formula_of ctx shared_ex
        (Boolean.mk_implies ctx
        (exists_formula_of ctx spec1_ex post1_formula)
        (exists_formula_of ctx spec2_ex post2_formula)
      )
  ] in
  let quanti_rhs = exists_formula_of ctx spec2_all impl_formula_rhs in
  let impl_formula = Boolean.mk_implies ctx impl_formula_lhs quanti_rhs in
  let quanti_formula = forall_formula_of ctx spec1_all impl_formula in
  (* let quanti_formula = Boolean.mk_and ctx
    [;
    quanti_formula] in *)
  Goal.add goal [quanti_formula];
  print_endline (Expr.to_string (quanti_formula));
  
(* Step 2: try high level verification *)
  let ho_verif = solver_check_bool ctx goal [] in
  if ho_verif then (Success) else
(
(* Step 3: try instantiate from user-defined predicates *)
  let fname_to_instantiate = 
    VarSet.elements
      (VarSet.union (fname_of_pures fun2.fpre.pure)
      (fname_of_pures (snd fun2.fpost).pure)) in
  let fname_sig_to_inst = List.map (Env.lookup_ftype env) fname_to_instantiate in
  let candidates = make_prop_candidates env fname_sig_to_inst in
  print_assignment_groups candidates;
  let feasible_candidates =
    List.filter (solver_check_bool ctx goal) candidates in
  if List.length feasible_candidates = 0 then Fail else
    Inst feasible_candidates
)

let make_goal ctx pre post =

  let pre_fvs = fvars_of_pures pre in
  let post_fvs = fvars_of_pures post in
  let post_exs = VarSet.diff post_fvs pre_fvs in 
  let forall_vars = VarSet.elements pre_fvs in
  let exists_vars = VarSet.elements post_exs in 

  let pre_formula = (pure_preds_to_expr ctx pre) in
  let post_formula = (pure_preds_to_expr ctx post) in


  (* let is = (Arithmetic.Integer.mk_sort ctx) in *)
  (* let forall_types = List.map (fun _ -> is) forall_vars in *)
  let forall_xs = List.map (fun v -> Expr.mk_const_s ctx v (Arithmetic.Integer.mk_sort ctx)) forall_vars in
  (* let exists_types = List.map (fun _ -> is) exists_vars in *)
  let exists_xs = List.map (fun v -> Expr.mk_const_s ctx v (Arithmetic.Integer.mk_sort ctx)) exists_vars in
  

  let post_formula2 = 
    if List.length exists_xs = 0 then post_formula else
    Quantifier.expr_of_quantifier (Quantifier.mk_exists_const ctx exists_xs post_formula 
                        (Some 1) [] [] (Some (Symbol.mk_string ctx "Qpost")) (Some (Symbol.mk_string ctx "skid2"))) in

  let impl_formula = Boolean.mk_implies ctx pre_formula post_formula2 in
  let impl_formula = 
    if List.length forall_xs = 0 then impl_formula else
    Quantifier.expr_of_quantifier (Quantifier.mk_forall_const ctx  forall_xs impl_formula 
                          (Some 1) [] [] (Some (Symbol.mk_string ctx "Qimpl")) (Some (Symbol.mk_string ctx "skid2"))) in
  (* let impl_formula = (Boolean.mk_and ctx [Quantifier.expr_of_quantifier pre_formula; Boolean.mk_not ctx post_formula]) in *)
  (* let impl_formula = Boolean.mk_or ctx [Boolean.mk_not ctx pre_formula; post_formula] in *)
  (* Goal.add goal [ impl_formula ]; *)
  impl_formula

let check_pure env (pre: pure_pred list) (post:pure_pred list) : bool = 
  print_endline "sleek: checking for pre and post";
  print_endline ("[pre] " ^ pure_preds_to_string pre);
  print_endline ("[post] " ^ pure_preds_to_string post);

  let cfg = [("model", "true"); ("proof", "true")] in
  let ctx = (mk_context cfg) in
  let goal = Goal.mk_goal ctx true true true in 

  let impl_formula = make_goal ctx pre post in

  Goal.add goal [impl_formula];
  let res = solver_check_bool ctx goal [] in
  if res then true else 

  ((* Step 3: try instantiate from user-defined predicates *)
  let fname_to_instantiate = 
    list_diff
      (VarSet.elements
        (VarSet.union (fname_of_pures pre)
        (fname_of_pures post)))
      (Env.insted_preds env) in
  let fname_sig_to_inst = List.map (Env.lookup_ftype env) fname_to_instantiate in
  let candidates = make_prop_candidates env fname_sig_to_inst in
  print_assignment_groups candidates;
  let check_candidate candidate =
    let inst_post = instantiate_pure_preds candidate post in
    let cfg = [("model", "true"); ("proof", "true")] in
    let ctx = (mk_context cfg) in
    let goal = Goal.mk_goal ctx true true true in 

    let impl_formula = make_goal ctx pre inst_post in

    Goal.add goal [impl_formula];
    solver_check_bool ctx goal []
  in

  let feasible_candidates =
    List.filter check_candidate candidates in
  if List.length feasible_candidates = 0 then false else
    true
)




(* let main () = 
  (* let pre = False in *)
  let pre = And ((And (Arith (Eq, (Pvar "x"), Const (Int 2)), Arith (Le, (Pvar "y"), Const (Int 6)))),
                  (Prop ("f", [Pvar "x"; Pvar "y"; Pvar "z"]))) in
  (* let post = True in *)
  let post = And (And ((Arith (Eq, (Pvar "x"), Const (Int 2))), Arith (Le, (Pvar "res"), Const (Int 6))), 
                (Prop ("f", [Const (Int 2); Pvar "y"; Pvar "z"]))) in
  print_endline (string_of_bool (check_pure [pre] [post])) *)
