open List
open Spectree
(* open Pretty *)
open Z3
exception TestFailedException of string

let print_length xs = print_endline ("Length is :" ^ string_of_int (List.length xs))


let encode_ty env ctx = function
| Int -> Some (Arithmetic.Integer.mk_sort ctx)
| Bool -> Some (Boolean.mk_sort ctx)
| Arrow _ -> 
    Some (Sort.mk_uninterpreted_s ctx ("foo"))
    (* None *)
| Tvar a -> 
    let tsort = Env.lookup_adt_sort env a in
    Some (tsort ctx)

let encode_arg env ctx = function
| (v, Int) -> Some (Arithmetic.Integer.mk_const_s ctx v)
| (v, Bool) -> Some (Boolean.mk_const_s ctx v)
| (_, Arrow _) -> None
| (v, Tvar a) ->
    let tsort = Env.lookup_adt_sort env a in
    Some (Expr.mk_const_s ctx v (tsort ctx))


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

let list_union a b =
  let a_set = VarSet.of_list a in
  let b_set = VarSet.of_list b in
  let a_union_b = VarSet.union a_set b_set in
  VarSet.elements a_union_b

let list_intersect a b =
  let a_set = VarSet.of_list a in
  let b_set = VarSet.of_list b in
  let a_set_b = VarSet.inter a_set b_set in
  VarSet.elements a_set_b


let forall_formula_of env ctx xs formula =
  let xs = List.filter_map (fun v -> 
      let vty = Env.lookup_vtype env v in
      match (encode_ty env ctx vty) with
      | Some ty ->  Some (Expr.mk_const_s ctx v ty)
      | None -> None ) xs in
  if List.length xs = 0 then formula else
  Quantifier.expr_of_quantifier (Quantifier.mk_forall_const ctx xs formula 
                        (Some 1) [] [] None None) 

let exists_formula_of env ctx xs formula =
  let xs = List.filter_map (fun v -> 
      let vty = Env.lookup_vtype env v in
      match (encode_ty env ctx vty) with
      | Some ty ->  Some (Expr.mk_const_s ctx v ty)
      | None -> None ) xs in
  if List.length xs = 0 then formula else
  Quantifier.expr_of_quantifier (Quantifier.mk_exists_const ctx xs formula 
                        (Some 1) [] [] None None) 

let forall_formula_of_ty env ctx xs formula =
  let xs = List.filter_map (fun (v, vty) -> 
    match (encode_ty env ctx vty) with
      | Some ty ->  Some (Expr.mk_const_s ctx v ty)
      | None -> None ) xs in
  if List.length xs = 0 then formula else
  Quantifier.expr_of_quantifier (Quantifier.mk_forall_const ctx xs formula 
                        (Some 1) [] [] None None) 
                        
let exists_formula_of_ty env ctx xs formula =
  let xs = List.filter_map (fun (v, vty) -> 
    match (encode_ty env ctx vty) with
      | Some ty ->  Some (Expr.mk_const_s ctx v ty)
      | None -> None ) xs in
  if List.length xs = 0 then formula else
  Quantifier.expr_of_quantifier (Quantifier.mk_exists_const ctx xs formula 
                        (Some 1) [] [] None None) 

let rec expr_to_expr env ctx exp : Expr.expr = 
  print_endline (logical_exp_to_string exp);
  match exp with
  | Pvar v  -> 
      let vty = Env.lookup_vtype env v in
      (match (encode_ty env ctx vty) with
      | Some ty -> Expr.mk_const_s ctx v ty
      | None -> failwith (v ^ " can't have function type"))
  | Lvar v -> 
      let vty = Env.lookup_vtype env v in
      (match (encode_ty env ctx vty) with
      | Some ty -> Expr.mk_const_s ctx v ty
      | None -> failwith (v ^ " can't have function type"))
  (* | Fun (f, vs) -> 
      let arg_list = List.init (List.length vs) (fun _ -> (Arithmetic.Integer.mk_sort ctx)) in
      let target_sort = Arithmetic.Integer.mk_sort ctx in
      let fun_decl = FuncDecl.mk_func_decl_s ctx f arg_list target_sort in
      let fun_args = 
        List.map (expr_to_expr ctx) vs  in
      Expr.mk_app ctx fun_decl fun_args *)
  | Const (Int n) -> Arithmetic.Integer.mk_numeral_i ctx n
  | Const (Bool i) -> if i then Boolean.mk_true ctx else Boolean.mk_false ctx
  | Op (oper , t1, t2) -> 
    let t1_expr = expr_to_expr env ctx t1 in
    let t2_expr = expr_to_expr env ctx t2 in
    match oper with 
  | Plus -> Arithmetic.mk_add ctx [t1_expr ; t2_expr ]
  | Minus -> Arithmetic.mk_sub ctx [t1_expr ; t2_expr ]
  | Mult -> Arithmetic.mk_mul ctx [t1_expr ; t2_expr ]
  | Eqb -> Boolean.mk_eq ctx t1_expr t2_expr

let rec pure_pred_to_expr env ctx pred : Expr.expr =
  let thinned_pred = thin_pred pred in
  match thinned_pred with
  | Arith (oper, t1, t2) ->
      let t1_exp = expr_to_expr env ctx t1 in
      let t2_exp = expr_to_expr env ctx t2 in
      let oper_fun = match oper with
      | Eq -> Boolean.mk_eq
        (* fun ctx e1 e2 -> Boolean.mk_and ctx [Arithmetic.mk_le ctx e1 e2 ; Arithmetic.mk_le ctx e2 e1 ] *)
      | Le -> Arithmetic.mk_le in (* TODO: is this correct? *)
      oper_fun ctx t1_exp t2_exp
  | Field (p, c, xs) ->
      let xs_exp = List.map (expr_to_expr env ctx) xs in
      let constr_decl = (Env.lookup_adt_constr env c) ctx in
      let p_sort = (Env.lookup_adt_sort env p) ctx in
      let p_expr = Expr.mk_const_s ctx p p_sort in
      Boolean.mk_eq ctx p_expr (FuncDecl.apply
        (Datatype.Constructor.get_constructor_decl constr_decl) xs_exp)
  | And (p1, p2) ->
      let p1_exp = pure_pred_to_expr env ctx p1 in
      let p2_exp = pure_pred_to_expr env ctx p2 in
      Boolean.mk_and ctx [p1_exp; p2_exp]
  (* | Or (p1, p2) -> 
    let p1_exp = pure_pred_to_goal ctx p1 in
    let p2_exp = pure_pred_to_goal ctx p2 in
      Boolean.mk_or ctx [p1_exp; p2_exp] *)
  | Neg p ->
      let p = pure_pred_to_expr env ctx p in
      Boolean.mk_not ctx p
  | Prop (p, vs) ->
      let arg_list = List.map (fun v -> Option.get (encode_ty env ctx (v))) (snd (Env.lookup_ftype env p)) in
      let target_sort = Boolean.mk_sort ctx in
      let fun_decl = FuncDecl.mk_func_decl_s ctx p arg_list target_sort in
      let fun_args = 
        List.map (expr_to_expr env ctx) vs  in
      Expr.mk_app ctx fun_decl fun_args
  | True -> Boolean.mk_true ctx
  | False -> Boolean.mk_false ctx

let pure_preds_to_expr env ctx preds : Expr.expr =
  let fold_or pred expr =
    (* print_endline (__LOC__);
    print_endline (Env.all_adts env ctx); *)
    let pred_expr = pure_pred_to_expr env ctx pred in
    Boolean.mk_or ctx [pred_expr; expr] in
  List.fold_right fold_or preds (Boolean.mk_false ctx) 

let ext_pure_preds_to_expr env ctx preds : Expr.expr =
  let fold_or (args, pred) expr =
    let pred_expr = pure_pred_to_expr env ctx pred in
    let pred_quanti = exists_formula_of_ty env ctx args pred_expr in
    Boolean.mk_or ctx [pred_quanti; expr] in
  List.fold_right fold_or preds (Boolean.mk_false ctx) 

let solver_wrapper ctx goal : bool =
  (* discharge a goal by solving its negation
     so that uninterpreted functions can be universally quantified on toplevel
  *)
  let solver = (Solver.mk_simple_solver ctx) in
    let f e = (Solver.add solver [ Boolean.mk_not ctx e ]) in
      ignore (List.map f (Goal.get_formulas goal)) ;
    let q = (Solver.check solver []) in
      if q == UNSATISFIABLE then 
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


let logical_proposition_to_expr env ctx fname {pargs; pbody; _} : Expr.expr =
  let arg_list = List.map fst pargs in
  let arg_ty_list = List.filter_map ((encode_ty env ctx)) (List.map (snd) pargs) in
  let var_list = List.filter_map (encode_arg env ctx) pargs in
  let ex_vars = list_diff (VarSet.elements (fvars_of_pures (List.map snd pbody))) arg_list in
  let fun_decl = FuncDecl.mk_func_decl_s ctx fname arg_ty_list (Boolean.mk_sort ctx) in
  forall_formula_of env ctx arg_list 
    (Boolean.mk_eq ctx 
      (exists_formula_of env ctx ex_vars (pure_preds_to_expr env ctx (List.map snd pbody)))
      (FuncDecl.apply fun_decl var_list) 
      )


let logical_proposition_to_expr_imply env ctx fname {pargs; pbody; _} : Expr.expr =
  (* let arg_list = List.map fst pargs in *)
  let ty_info = pargs @ (List.concat_map fst pbody) in
  let env = List.fold_left (fun env (argv, argt) -> Env.add_vtype env argv argt) env ty_info in
  let arg_ty_list = List.filter_map ((encode_ty env ctx)) (List.map (snd) pargs) in
  let var_list = List.filter_map (encode_arg env ctx) pargs in
  (* let ex_vars = list_diff (VarSet.elements (fvars_of_pures (List.map snd pbody))) arg_list in *)
  let fun_decl = FuncDecl.mk_func_decl_s ctx fname arg_ty_list (Boolean.mk_sort ctx) in
  forall_formula_of_ty env ctx pargs 
    (Boolean.mk_implies ctx 
      ((ext_pure_preds_to_expr env ctx (pbody)))
      (FuncDecl.apply fun_decl var_list) 
      )

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



let solver_check_bool env ctx goal (cand: prop_candidates) =
  print_length(cand);
  let solver = (Solver.mk_simple_solver ctx) in
    let f e = 
      (* (print_endline (Expr.to_string (Boolean.mk_not ctx e)); *)
    Solver.add solver [ Boolean.mk_not ctx e ] in
      ignore (List.map f (Goal.get_formulas goal)) ;
    List.iter (fun (fname, fbody) ->
      let post_extv = List.fold_right (@) (List.map fst (fbody.pbody)) [] in
      let post_extv = fbody.pargs @ post_extv in
      let env' = List.fold_left (fun env (v, vty) -> Env.add_vtype env v vty) env post_extv in
      print_endline (Expr.to_string (logical_proposition_to_expr env' ctx fname fbody));
      Solver.add solver
      [logical_proposition_to_expr env' ctx fname fbody]) cand;
    let q = (Solver.check solver []) in
      if q == UNSATISFIABLE then true
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



let solver_check_bool_imply env ctx goal (cand: prop_candidates) =
  let solver = (Solver.mk_simple_solver ctx) in
    let f e = 
      (* (print_endline (Expr.to_string (Boolean.mk_not ctx e)); *)
    Solver.add solver [ Boolean.mk_not ctx e ] in
      ignore (List.map f (Goal.get_formulas goal)) ;
    List.iter (fun (fname, fbody) -> 
      print_endline (Expr.to_string (logical_proposition_to_expr_imply env ctx fname fbody));
      Solver.add solver
      [logical_proposition_to_expr_imply env ctx fname fbody]) cand;
    let q = (Solver.check solver []) in
      if q == UNSATISFIABLE then true
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
        

let var_quantifier_of_spec (spec: fun_signature) : (logical_var ) list *  (logical_var) list =
  let pre = spec.fpre.pure in
  let post = (snd spec.fpost).pure in
  let fvars = VarSet.of_list (List.map fst spec.fvar) in
  (* TODO: sanity check *)
  let pre_fvs = fvars_of_pures  (List.map snd pre ) in
  let post_fvs = fvars_of_pures (List.map snd post)  in
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
    fpre = subst_pred_normal_forms (List.map fst fun2.fvar) (List.map fst fun1.fvar) fun2.fpre;
    fpost = (fst fun1.fpost ,
      let subst_ret = subst_pred_normal_form 
        (fst (fst fun2.fpost)) (fst (fst fun1.fpost)) (snd fun2.fpost) in
      subst_pred_normal_forms (List.map fst fun2.fvar) (List.map fst fun1.fvar) subst_ret)
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



let check_spec_sub (env:env) (pre: ( pred) list) fun1 fun2 : spec_res = 
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
  print_endline (Env.vtypes_to_string env );
  print_endline ("[context] " ^ pure_preds_to_string pre);
  print_endline ("[client] " ^ string_of_fun_spec fun1);
  print_endline ("[server] " ^ string_of_fun_spec fun2);

(* Step 1: translate to Z3 expression *)
  let fun2 = rename_fun_args fun1 fun2 in
  (* fun1 is also associated with pre, so we rename fun2 *)

  let post_extv = List.fold_right (@) (List.map fst 
    (fun1.fpre.pure @ (snd fun1.fpost).pure @ fun2.fpre.pure @ (snd fun2.fpost).pure  )) [] in
  let post_extv = [fst fun1.fpost] @ fun1.fvar @ post_extv in
  let env = List.fold_left (fun env (v, vty) -> Env.add_vtype env v vty) env post_extv in

  (*
  g:{ex x'.P'(x')} r {ex y'. Q'(x',y',r)} <: f(Z):{ ex x. P(x) } r { ex y. Q(x,y,r) } 
  ==>
  forall x, P(x) -> ex Z', ex x'. P'(x') /\ (forall r, ex y'. Q'(x',y',r) -> ex y. Q(x,y,r))
  *)
  let x' = List.concat_map fst fun1.fpre.pure in
  let x = List.concat_map fst fun2.fpre.pure in
  let y' = List.concat_map fst (snd fun1.fpost).pure in
  let y = List.concat_map fst (snd fun2.fpost).pure in
  (* let spec1_all, spec1_ex = var_quantifier_of_spec fun1 in
  let spec2_all, spec2_ex = var_quantifier_of_spec fun2 in
  let fvar_pre = VarSet.elements (fvars_of_pures (pre)) in
  let spec1_all = list_diff spec1_all fvar_pre in
  let spec2_all = list_diff spec2_all fvar_pre in
  let arg_all = list_union fvar_pre (List.map fst fun1.fvar) in
  let spec1_all = list_diff spec1_all arg_all in
  let spec2_all = list_diff spec2_all arg_all in
  let shared_ex = list_intersect spec1_ex spec2_ex in
  let spec1_ex = list_diff spec1_ex shared_ex in
  let spec2_ex = list_diff spec2_ex shared_ex in *)

  let cfg = [("model", "true"); ("proof", "true")] in
  let ctx = (mk_context cfg) in
  let goal = Goal.mk_goal ctx true true true in 
  (*print_endline (pure_preds_to_string pre); *)
  let pre_formula = (pure_preds_to_expr env ctx pre) in
  let pre1_formula = (pure_preds_to_expr env ctx (List.map snd fun1.fpre.pure)) in
  let pre2_formula = (pure_preds_to_expr env ctx (List.map snd fun2.fpre.pure)) in
  print_endline __LOC__;
  let post1_formula = (pure_preds_to_expr env ctx (List.map snd (snd fun1.fpost).pure)) in
  let post2_formula = (pure_preds_to_expr env ctx (List.map snd (snd fun2.fpost).pure)) in
  let impl_formula_lhs = Boolean.mk_and ctx [pre_formula; pre2_formula] in
  let impl_formula_rhs = Boolean.mk_and ctx [
      pre1_formula;
      (* forall_formula_of env ctx shared_ex *)
        (Boolean.mk_implies ctx
        (exists_formula_of_ty env ctx y' post1_formula)
        (exists_formula_of_ty env ctx y post2_formula)
      )
  ] in
  let quanti_rhs = exists_formula_of_ty env ctx x' impl_formula_rhs in
  let impl_formula = Boolean.mk_implies ctx impl_formula_lhs quanti_rhs in
  let quanti_formula = forall_formula_of_ty env ctx x impl_formula in
  (* let quanti_formula = Boolean.mk_and ctx
    [;
    quanti_formula] in *)
  Goal.add goal [quanti_formula];
  print_endline (__LOC__ ^ Expr.to_string (quanti_formula));
  
(* Step 2: try high level verification *)
  let ho_verif = solver_check_bool env ctx goal [] in
  if ho_verif then (Success) else
(
(* Step 3: try instantiate from user-defined predicates *)
  let fname_to_instantiate = 
    VarSet.elements
      (VarSet.union (fname_of_pures (List.map snd fun2.fpre.pure))
      (fname_of_pures (List.map snd (snd fun2.fpost).pure))) in
      print_string ("inst "); print_length(fname_to_instantiate);
      print_endline (string_of_fun_spec fun2);
      print_endline (string_of_fun_spec fun1);
  let fname_sig_to_inst = List.map (Env.lookup_ftype env) fname_to_instantiate in
  let candidates = make_prop_candidates env fname_sig_to_inst in
  print_assignment_groups candidates;
  let feasible_candidates =
    List.filter (solver_check_bool env ctx goal) candidates in
  if List.length feasible_candidates = 0 then Fail else
    Inst feasible_candidates
)

let make_goal env ctx (pre:( pred) list) 
(post:((program_var * exp_type) list * pred) list) prog_vars =

  let post_extv = List.fold_right (@) (List.map fst post) [] in
  let env = List.fold_left (fun env (v, vty) -> Env.add_vtype env v vty) env post_extv in

  let forall_prog_vs = VarSet.of_list prog_vars in
  let pre_fvs = VarSet.diff (fvars_of_pures (pre)) forall_prog_vs in
  let post_fvs = VarSet.diff (fvars_of_pures (List.map snd post)) forall_prog_vs in
  let post_exs = VarSet.diff post_fvs pre_fvs in 
  let forall_vars = VarSet.elements pre_fvs in
  let exists_vars = VarSet.elements post_exs in 

  let pre_formula = (pure_preds_to_expr env ctx pre) in
  let post_formula = (ext_pure_preds_to_expr env ctx post) in


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

let check_pure env (pre: (pred) list) 
                   (post:((program_var * exp_type) list * pred) list) : bool = 
  print_endline "sleek: checking for pre and post";
  print_endline (Env.vtypes_to_string env);
  print_endline ("[pre] " ^ pure_preds_to_string pre);
  print_endline ("[post] " ^ ext_pure_preds_to_string post);


  let cfg = [("model", "true"); ("proof", "true")] in
  let ctx = (mk_context cfg) in
  let goal = Goal.mk_goal ctx true true true in 

  let impl_formula = make_goal env ctx pre post (env.prog_vars) in

  Goal.add goal [impl_formula];
  let res = solver_check_bool env ctx goal [] in
  if res then true else 
    

  ((* Step 3: try instantiate from user-defined predicates *)
  let fname_to_instantiate = 
    list_diff
      (VarSet.elements
        (VarSet.union (fname_of_pures (pre))
        (fname_of_pures (List.map snd post))))
      (Env.insted_preds env) in
    (* List.iter (fun v -> print_endline __LOC__; print_endline v) fname_to_instantiate; *)
    (* print_endline (__LOC__ ^ string_of_int (List.length (SMap.bindings env.ftype_context))); *)
  let fname_sig_to_inst = List.map (Env.lookup_ftype env) fname_to_instantiate in
  let candidates = make_prop_candidates env fname_sig_to_inst in
  print_assignment_groups candidates;
  let check_candidate candidate =
    (* let candidate_constraints = List.map (fun (inst_name, candidate) ->
        logical_proposition_to_expr_imply ctx inst_name candidate
    ) candidate in *)

    
    (* let inst_pre = instantiate_pure_preds candidate pre in *)
    (* let inst_post = instantiate_pure_preds candidate post in *)


    let cfg = [("model", "true"); ("proof", "true")] in
    let ctx = (mk_context cfg) in
    let goal = Goal.mk_goal ctx true true true in 

    let impl_formula = make_goal env ctx pre post (env.prog_vars) in

    Goal.add goal [impl_formula];
    print_endline (Z3.Expr.to_string impl_formula);
    solver_check_bool_imply env ctx goal candidate
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
