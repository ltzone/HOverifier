open List
open Spectree
(* open Pretty *)
open Z3
exception TestFailedException of string



let rec expr_to_expr ctx : logical_exp -> Expr.expr = function
  | Pvar v  -> Arithmetic.Integer.mk_const_s ctx v
  | Lvar v -> Arithmetic.Integer.mk_const_s ctx v
  | Fun (f, vs) -> 
      let arg_list = List.init (List.length vs) (fun _ -> (Arithmetic.Integer.mk_sort ctx)) in
      let target_sort = Arithmetic.Integer.mk_sort ctx in
      let fun_decl = FuncDecl.mk_func_decl_s ctx f arg_list target_sort in
      let fun_args = 
        List.map (expr_to_expr ctx) vs  in
      Expr.mk_app ctx fun_decl fun_args
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
      let target_sort = Arithmetic.Integer.mk_sort ctx in
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
                  (Arith (Eq, Fun ("f", [Pvar "x"; Pvar "y"]), Pvar "z"))) in
  (* let post = True in *)
  let post = And (And ((Arith (Eq, (Pvar "x"), Const (Int 2))), Arith (Le, (Pvar "res"), Const (Int 6))), 
                Arith (Eq, Pvar "z", Fun ("f", [Const (Int 3); Pvar "y"]))) in
  ignore (check_pure [pre] [post])
