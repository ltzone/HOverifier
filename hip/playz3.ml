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
      Printf.printf "Model: \n%s\n" (Model.to_string m) 


let model_converter_test (ctx: context) =
  let xr = (Expr.mk_const ctx (Symbol.mk_string ctx "x") (Arithmetic.Real.mk_sort ctx)) in
  let yr = (Expr.mk_const ctx (Symbol.mk_string ctx "y") (Arithmetic.Real.mk_sort ctx)) in
  let g4 = Goal.mk_goal ctx true true true in 
    (* model generation / unsat core generation / proof generation *)
  (Goal.add g4 [ (Arithmetic.mk_gt ctx (Arithmetic.Real.mk_numeral_nd ctx 1 1) xr )]);
  (Goal.add g4 [ (Boolean.mk_eq ctx yr (Arithmetic.mk_add ctx [xr; Arithmetic.Real.mk_numeral_nd ctx 1 1]))]);
  (Goal.add g4 [ (Arithmetic.mk_gt ctx yr (Arithmetic.Real.mk_numeral_nd ctx 1 1))]);
  solver_wrapper ctx g4

let main () =
	let cfg = [("model", "true"); ("proof", "true")] in
	let ctx = (mk_context cfg) in
  model_converter_test ctx