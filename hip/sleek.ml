open List
open Spectree
(* open Pretty *)
open Z3
exception TestFailedException of string

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
        
let solver_check_bool ctx goal =
  let solver = (Solver.mk_simple_solver ctx) in
    let f e = 
      (* (print_endline (Expr.to_string (Boolean.mk_not ctx e)); *)
    Solver.add solver [ Boolean.mk_not ctx e ] in
      ignore (List.map f (Goal.get_formulas goal)) ;
    Solver.add solver [((forall_formula_of ctx ["w1";"w2"]
    (Boolean.mk_eq  ctx ( FuncDecl.apply
      (FuncDecl.mk_func_decl_s ctx "fpure" [
        Arithmetic.Integer.mk_sort ctx;
        Arithmetic.Integer.mk_sort ctx
      ] (Boolean.mk_sort ctx))
      [(Arithmetic.Integer.mk_const_s ctx "w1");
      (Arithmetic.Integer.mk_const_s ctx "w2")]
    ) (Boolean.mk_eq ctx (Arithmetic.Integer.mk_const_s ctx "w1")
    (Arithmetic.Integer.mk_const_s ctx "w2")))
  ))];
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
  let pre_fvs = fvars_of_pures pre in
  let post_fvs = fvars_of_pures post in
  let post_exs = VarSet.diff post_fvs pre_fvs in 
  let forall_vars = VarSet.elements pre_fvs in
  let exists_vars = VarSet.elements post_exs in 
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

let list_diff a b =
  let a_set = VarSet.of_list a in
  let b_set = VarSet.of_list b in
  let a_diff_b = VarSet.diff a_set b_set in
  VarSet.elements a_diff_b

type spec_res = 
| Fail
| Success
| Inst of logical_proposition

let check_spec_sub (pre: pure_pred list) fun1 fun2 : bool = 
(* For client signature GIVEN Z1, pre1 *-> exist x.post1
   and server signature GIVEN Z2, pre2 *-> exist y.post2
   fun1 <: fun2 should be encoded into

   Forall Z1, pre /\ pre1 -> 
    (Exists Z2, pre2 /\
      (exists y. post2 -> exists x. post1))

   To make SMT work, Z2 needs to be instantiated in advance
   so that we can use negation technique to check the globally
   quantified Z1
*)
  print_endline "sleek: checking for subsumption";
  print_endline ("[context] " ^ pure_preds_to_string pre);
  print_endline ("[client] " ^ string_of_fun_spec fun1);
  print_endline ("[server] " ^ string_of_fun_spec fun2);


  let fun2 = rename_fun_args fun1 fun2 in
  (* fun1 is also associated with pre, so we rename fun2 *)

  let spec1_all, spec1_ex = var_quantifier_of_spec fun1 in
  let spec2_all, spec2_ex = var_quantifier_of_spec fun2 in
  let arg_all = fun1.fvar in
  let spec1_all = list_diff spec1_all arg_all in
  let spec2_all = list_diff spec2_all arg_all in

  let cfg = [("model", "true"); ("proof", "true")] in
  let ctx = (mk_context cfg) in
  let goal = Goal.mk_goal ctx true true true in 
  let pre_formula = (pure_preds_to_expr ctx pre) in
  let pre1_formula = (pure_preds_to_expr ctx fun1.fpre.pure) in
  let pre2_formula = (pure_preds_to_expr ctx fun2.fpre.pure) in
  let post1_formula = (pure_preds_to_expr ctx (snd fun1.fpost).pure) in
  let post2_formula = (pure_preds_to_expr ctx (snd fun2.fpost).pure) in
  let impl_formula_lhs = Boolean.mk_and ctx [pre_formula; pre1_formula] in
  let impl_formula_rhs = Boolean.mk_and ctx [
      pre2_formula;
      Boolean.mk_implies ctx
        (exists_formula_of ctx spec2_ex post2_formula)
        (exists_formula_of ctx spec1_ex post1_formula)
  ] in
  let quanti_rhs = exists_formula_of ctx spec2_all impl_formula_rhs in
  let impl_formula = Boolean.mk_implies ctx impl_formula_lhs quanti_rhs in
  let quanti_formula = forall_formula_of ctx spec1_all impl_formula in
  (* let quanti_formula = Boolean.mk_and ctx
    [;
    quanti_formula] in *)
  Goal.add goal [quanti_formula];
  print_endline (Expr.to_string (quanti_formula));
  (* let quantified_formula = Quantifier.mk_forall_const ctx  in *)
  let res = solver_check_bool ctx goal in
  (* print_endline "finished checking";  *)
  res


let check_pure (pre: pure_pred list) (post:pure_pred list) : bool = 
  print_endline "sleek: checking for pre and post";
  print_endline ("[pre] " ^ pure_preds_to_string pre);
  print_endline ("[post] " ^ pure_preds_to_string post);

  let pre_fvs = fvars_of_pures pre in
  let post_fvs = fvars_of_pures post in
  let post_exs = VarSet.diff post_fvs pre_fvs in 
  let forall_vars = VarSet.elements pre_fvs in
  let exists_vars = VarSet.elements post_exs in 

  let cfg = [("model", "true"); ("proof", "true")] in
  let ctx = (mk_context cfg) in
  let goal = Goal.mk_goal ctx true true true in 
  let pre_formula = (pure_preds_to_expr ctx pre) in
  let post_formula = (pure_preds_to_expr ctx post) in


  (* let pre_fname = fname_of_pure pre VarSet.empty in *)
  (* let post_fname = fname_of_pure post VarSet.empty in *)
  (* let fname_to_instantiate = VarSet.diff pre_fname post_fname in *)
  (* print_endline ("sleek: need to instantiate: " ^ String.concat "," (VarSet.elements fname_to_instantiate) ); *)


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
  Goal.add goal [impl_formula];
  (* print_endline (Expr.to_string (impl_formula)); *)
  (* let quantified_formula = Quantifier.mk_forall_const ctx  in *)
  let res = solver_check_bool ctx goal in
  (* print_endline "finished checking";  *)
  res



let main () = 
  (* let pre = False in *)
  let pre = And ((And (Arith (Eq, (Pvar "x"), Const (Int 2)), Arith (Le, (Pvar "y"), Const (Int 6)))),
                  (Prop ("f", [Pvar "x"; Pvar "y"; Pvar "z"]))) in
  (* let post = True in *)
  let post = And (And ((Arith (Eq, (Pvar "x"), Const (Int 2))), Arith (Le, (Pvar "res"), Const (Int 6))), 
                (Prop ("f", [Const (Int 2); Pvar "y"; Pvar "z"]))) in
  print_endline (string_of_bool (check_pure [pre] [post]))
