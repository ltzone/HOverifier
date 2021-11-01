open Z3

exception TestFailedException of string



let solver_wrapper ctx goal =
  let solver = (Solver.mk_simple_solver ctx) in
    let f e = (Solver.add solver [ e ]) in
      ignore (List.map f (Goal.get_formulas goal)) ;
    let q = (Solver.check solver []) in
      if q == UNSATISFIABLE then 
        (print_endline (Expr.to_string (Option.get  (Solver.get_proof solver)));
        print_endline "success")
        (* raise (TestFailedException "") *)
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
  let bool_sort = Boolean.mk_sort ctx in
  (* let x = (Expr.mk_const ctx (Symbol.mk_string ctx "x") int_sort) in
  let y = (Expr.mk_const ctx (Symbol.mk_string ctx "y") int_sort) in *)
  let q1 = (Expr.mk_const ctx (Symbol.mk_string ctx "q1") int_sort) in
  let q2 = (Expr.mk_const ctx (Symbol.mk_string ctx "q2") int_sort) in
  let r_ex = (Expr.mk_const ctx (Symbol.mk_string ctx "r_ex") int_sort) in
  let one = (Arithmetic.Integer.mk_numeral_i ctx 1) in
  let mk_num x = (Arithmetic.Integer.mk_numeral_i ctx x) in
  let zero = (Arithmetic.Integer.mk_numeral_i ctx 0) in
  let factP = (FuncDecl.mk_func_decl_s ctx "fpure" [int_sort; int_sort] bool_sort) in


  (*  rule for fold p *)
  let formula =
    (Boolean.mk_implies ctx    
      (
        Boolean.mk_or ctx [
          (Boolean.mk_and ctx [
            Boolean.mk_eq ctx q1 zero;
            Boolean.mk_eq ctx q2 one;
          ]);
          (Quantifier.expr_of_quantifier (
            Quantifier.mk_exists_const ctx [r_ex] (
              Boolean.mk_and ctx [
                Boolean.mk_eq ctx (Arithmetic.mk_mul ctx [r_ex; q1]) q2;
                FuncDecl.apply factP [ (Arithmetic.mk_sub ctx [q1; one]) ; r_ex]
              ]
            ) (Some 1) [] [] None None
          ));
        ] 
      )
    ( FuncDecl.apply factP [q1; q2] )
  ) in
  let quantified_formula = 
    Quantifier.mk_forall_const ctx [q1; q2] formula (Some 1) [] [] None None in

  let entail_goal = FuncDecl.apply factP [mk_num 0; mk_num 2] in
  let entail_goal = Boolean.mk_not ctx entail_goal in

  let comb = Boolean.mk_and ctx [(Quantifier.expr_of_quantifier quantified_formula);
  entail_goal] in

  print_endline (Expr.to_string comb);

  let g4 = Goal.mk_goal ctx true true true in 
    (* model generation / unsat core generation / proof generation *)
  (Goal.add g4 [ comb ] ;
  );
  print_endline "testing";
  solver_wrapper ctx g4



let main =
	let cfg = [("model", "true"); ("proof", "true")] in
	let ctx = (mk_context cfg) in
  find_expr_test ctx