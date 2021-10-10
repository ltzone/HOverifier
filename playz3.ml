open Z3

exception TestFailedException of string

let solver_wrapper ctx goal =
  let solver = (Solver.mk_simple_solver ctx) in
    let f e = (Solver.add solver [ e ]) in
      ignore (List.map f (Goal.get_formulas goal)) ;
    let q = (Solver.check solver []) in
      if q != SATISFIABLE then 
        raise (TestFailedException "")
      else
        let m = (Solver.get_model solver) in    
        match m with 
    | None -> 
      raise (TestFailedException "")
    | Some (m) -> 
      Printf.printf "Solver says: %s\n" (Solver.string_of_status q) ;
      Printf.printf "Model: \n%s\n" (Model.to_string m);
      flush stdout


let model_converter_test (ctx: context) =
  let xr = (Expr.mk_const ctx (Symbol.mk_string ctx "x") (Arithmetic.Real.mk_sort ctx)) in
  let yr = (Expr.mk_const ctx (Symbol.mk_string ctx "y") (Arithmetic.Real.mk_sort ctx)) in
  let g4 = Goal.mk_goal ctx true true true in 
    (* model generation / unsat core generation / proof generation *)
  (Goal.add g4 [ (Arithmetic.mk_gt ctx (Arithmetic.Real.mk_numeral_nd ctx 1 1) xr )]);
  (Goal.add g4 [ (Boolean.mk_eq ctx yr (Arithmetic.mk_add ctx [xr; Arithmetic.Real.mk_numeral_nd ctx 1 1]))]);
  (Goal.add g4 [ (Arithmetic.mk_gt ctx yr (Arithmetic.Real.mk_numeral_nd ctx 1 1))]);
  solver_wrapper ctx g4

let find_expr_test (ctx:context) =
  let int_sort = Arithmetic.Integer.mk_sort ctx in
  let x = (Expr.mk_const ctx (Symbol.mk_string ctx "x") int_sort) in
  let y = (Expr.mk_const ctx (Symbol.mk_string ctx "y") int_sort) in
  let fpure = (FuncDecl.mk_func_decl_s ctx "fpure" [int_sort] int_sort) in

  let formula =
    (Boolean.mk_implies ctx
    (Boolean.mk_eq ctx y 
    (* (Arithmetic.mk_add ctx [x; x])) *)
    (Expr.mk_app ctx fpure [x]))
    (Boolean.mk_eq ctx y (Arithmetic.mk_add ctx [x; x])
  )) in
  let quantified_formula = 
    Quantifier.mk_forall_const ctx [x; y] formula (Some 1) [] [] None None in
  let g4 = Goal.mk_goal ctx true true true in 
    (* model generation / unsat core generation / proof generation *)
  (Goal.add g4 [ Quantifier.expr_of_quantifier quantified_formula ]);
  print_endline "testing";
  solver_wrapper ctx g4



let main =
	let cfg = [("model", "true"); ("proof", "true")] in
	let ctx = (mk_context cfg) in
  find_expr_test ctx