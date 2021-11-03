open Frontend;;


let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (String.trim line) :: input_lines file
  | _ -> failwith "Weird input_line return value"


let test () = 
  let module_name = 
    if Array.length (Sys.argv) <= 1 then
      "double"
    else Array.get Sys.argv 1 in
  let input_module = "testcases/" ^ module_name in
  let input_program = input_module ^ ".ml" in
  (* let input_spec = input_module ^ ".ml" in *)
  (* let env = make_sig_from_file input_spec in *)
  (* print_endline (__LOC__ ^ string_of_int (List.length (Hiphop.Spectree.SMap.bindings env.ftype_context))); *)
  let ic = open_in input_program in
  print_endline ("[TEST] " ^ module_name);
  let output = ref "\nResult:" in
  try
    let lines =  (input_lines ic ) in
    let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (List.rev lines) "" in
    

    let progs = Parser.implementation Lexer.token (Lexing.from_string line) in
    
    List.iter (fun prog -> (  

    Format.printf "%a@." Printast.implementation [prog];
      
        )) progs ;

    flush stdout;                (* 现在写入默认设备 *)
    close_in ic                  (* 关闭输入通道 *);
    print_endline !output;
    
  with e ->                      (* 一些不可预见的异常发生 *)
    close_in_noerr ic;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
   ;;

open Z3;;

let ctx : context = mk_context []

let solver : Solver.solver = Solver.mk_solver ctx None

let intSort : Sort.sort = Arithmetic.Integer.mk_sort ctx

let boolSort : Sort.sort = Boolean.mk_sort ctx

let intConstructor = Datatype.mk_constructor_s ctx "IntV"
    (Symbol.mk_string ctx "IntV") [Symbol.mk_string ctx "i"] [Some intSort] [0]
let boolConstructor = Datatype.mk_constructor_s ctx "BoolV"
    (Symbol.mk_string ctx "BoolV") [] [] []

let ibSort = Datatype.mk_sort_s ctx "IntOrBool"
    [intConstructor; boolConstructor]

let nil_constr = Datatype.mk_constructor_s ctx "Nil" (Symbol.mk_string ctx "Nil") [] [] []

let cons_constr = Datatype.mk_constructor_s ctx "Cons" (Symbol.mk_string ctx "Cons") 
[Symbol.mk_string ctx "Cons_1"; Symbol.mk_string ctx "Cons_2" ] 
[Some intSort; None]
[1; 0]

let lisSort = Datatype.mk_sort_s ctx "list"
  [nil_constr; cons_constr]

let e2 = Expr.mk_const_s ctx "random2" lisSort

let nilDecl = Datatype.Constructor.get_constructor_decl nil_constr

let intv2 = FuncDecl.apply nilDecl []

let consDecl = Datatype.Constructor.get_constructor_decl cons_constr

let intv3 = FuncDecl.apply consDecl [Expr.mk_numeral_int ctx 22 intSort; intv2]

let f2 = Boolean.mk_eq ctx intv2 intv3

let e1 = Expr.mk_const_s ctx "random" ibSort

let true_ = Boolean.mk_val ctx true

let intcDecl = Datatype.Constructor.get_constructor_decl intConstructor

let intv1 = FuncDecl.apply intcDecl [Expr.mk_numeral_int ctx 42 intSort]

let f1 = Boolean.mk_eq ctx e1 intv1
(* true_ *)

let () =
  Solver.add solver [f1; f2];
  print_endline @@ Solver.to_string solver;
  match Solver.check solver [] with
  | Solver.SATISFIABLE ->
    begin
      match Solver.get_model solver with
      | None -> print_endline "none"
      | Some model -> print_endline @@ Model.to_string model
    end
  | Solver.UNSATISFIABLE ->
    print_endline "UNSAT"
  | Solver.UNKNOWN ->
    failwith @@ Printf.sprintf "Unknown result in solve: %s"
      (Solver.get_reason_unknown solver)


(* (declare-datatypes ((IntOrBool 0)) (((IntV (i Int)) (BoolV))))
(declare-fun random () IntOrBool)
(assert (= random (IntV 42)))
(define-fun random () IntOrBool
  (IntV 42)) *)


(* let _ = test () *)