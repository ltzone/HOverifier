
module Parser = Frontend.Parser
module Lexer = Frontend.Lexer
module Printast = Frontend.Printast
module Parsetree = Frontend.Parsetree
open Spectree

let add_constraint_pure (acc_pure) (pred: pure_pred)  =
  List.map (fun preds -> And (pred, preds)) acc_pure


let add_constraint (acc: pred_normal_form) (pred: pure_pred) : pred_normal_form =
  { acc with pure= List.map (fun (xs, preds) -> xs, And (pred, preds)) (acc.pure) }


(* let rec repeat_try f env (candidates:'a)  =
  List.fold_right
  (fun candidate last_res ->
    let last_flag, last_env = last_res in
    if last_flag then
      (let this_env, this_flag = f last_env candidate in
        this_flag, this_env)
    else false, env
    ) candidates *)



let print_length xs = print_endline ("Length is :" ^ string_of_int (List.length xs))

let string_of_ident = Frontend.Longident.last

let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (String.trim line) :: input_lines file
  | _ -> failwith "Weird input_line return value"


let string_of_pattern :Parsetree.pattern_desc -> string = function
| Ppat_var v -> v.txt
| _ -> failwith "Patterns not implemented"

let collect_program_vars (rhs:Parsetree.expression) =
  let rec traverse_to_body (e:Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_fun (_, _, name, body) ->
      let name =
        match name.ppat_desc with
        | Ppat_var s -> [s.txt]
        | _ ->
          (* we don't currently recurse inside patterns to pull out variables, so something like

             let f () (Foo a) = 1

             will be treated as if it has no formal params. *)
          []
      in
      name @ traverse_to_body body
    | _ -> []
  in
  traverse_to_body rhs



let rec split_nth (n:int) prev xs =
  if n <= 0 then prev, xs else
   split_nth (n-1) (List.append prev [List.hd xs]) (List.tl xs)






(* let check_spec env spec_to_check : bool =
  let check_single_spec env spec = 
    let matched_spec = Env.find_spec spec.fname env in
      (* check whether spec_to_check is a subsumption of the matched_spec *)



;; *)

let findi_opt f xs =
  let rec findi_opt_aux n = function
  | y::ys -> if (f y) then Some n else findi_opt_aux (n+1) ys
  | [] -> None in
  findi_opt_aux 0 xs


let check_fun env args fvars spec_to_check (pre:pred list) : spec_res * env =
   List.iter (fun v -> print_endline (__LOC__ ^ string_of_fun_spec v)) spec_to_check;
  (*print_endline (__LOC__ ^ Env.all_specs env); *)
   
  let find_spec_name fname =
    findi_opt (String.equal fname) fvars in
  let check_single_fun_spec spec =
    match (find_spec_name spec.fname) with
    | None -> 
      (print_endline (string_of_fun_spec spec);
        (* print_endline (Env.all_specs env); *)
        match Env.find_spec (spec.fname) env with
        | Some (spec_def::_) -> print_endline __LOC__; Sleek.check_spec_sub env pre spec_def spec
        | _ -> failwith ("The specification declared as " ^ spec.fname ^ " is neither part of the argument list nor defined function \n"
        ^ string_of_fun_spec spec )
      )
    | Some i ->
        let fun_name_in_env = List.nth args i in
        let client_spec = Env.find_spec fun_name_in_env env in
        match client_spec with
        (* TODO: try multiple specifications *)
          | Some (client_spec :: _) ->  Sleek.check_spec_sub env pre client_spec spec
          | _ -> failwith ("No specification found for function " ^ fun_name_in_env)
    in
    (* print_endline ("Spec to check:::::" ^ string_of_int (List.length spec_to_check)); *)
  (* print_length (spec_to_check); *)
  let merge_two_inst vs vs' = 
    (* combine two fname assignments, if the assignment conflict, abort this pair *)
    let merged_inst = List.concat_map (fun v -> List.map ((@) v) vs') vs in
    let filter_fun assignment = 
      let assignment_map = Some SMap.empty in
      let mapped_res = 
        List.fold_right (fun (name, prop) old_map -> 
          match old_map with
          | Some old_map ->
            (match SMap.find_opt name old_map with
            | None -> Some (SMap.add name prop old_map)
            | Some {pname; _} ->
                if String.equal pname prop.pname then Some old_map else None)
          | None -> None
        ) assignment assignment_map in
      match mapped_res with None -> None | Some map -> Some (SMap.bindings map) in
    List.filter_map filter_fun merged_inst in


  let merged_spec_res = List.fold_right
  (fun spec last_res -> match last_res with Fail -> Fail
  | Success -> (check_single_fun_spec spec)
  | Inst vs -> match check_single_fun_spec spec with
    | Fail -> Fail
    | Success -> Inst vs
    | Inst vs' ->  (* TODO to be tested *)
          Inst (merge_two_inst vs vs')) spec_to_check Success in
  merged_spec_res, env


(* move the existantial vars & function specifications into the envrionment *)
let normalize_cond env acc =
  let pre_cond = acc.pure in
  (* introduce the existantial variables to the environment *)
  let pre_extv = List.fold_right (@) (List.map fst pre_cond) [] in
  let pre_cond = List.map snd pre_cond in
  let env = List.fold_left (fun env (v, vty) -> Env.add_vtype env v vty) env pre_extv in
  (* add function specifications *)
  let env = List.fold_left 
              (fun env spec -> Env.add_spec_to_fn spec.fname spec env)
              env acc.spec in  
    env, pre_cond

  (* TODO: first check partial/full application, then check pre-post SMT *)
let check_spec_derive env (pre_cond: pure_pred list) args (spec:fun_signature)  : (env * logical_var * pred_normal_form) list =
(* let spec_to_check = spec.fspec.fpre.spec in *)
(* let check_env = check_spec env spec_to_check in *)
(* if check_bool && check_env then  *)
  if List.length args = List.length spec.fvar then
    (* full application *)
    
    
    ((let subst_pre_to_check = subst_pred_normal_forms (List.map fst spec.fvar) args spec.fpre in
    (* print_endline (__LOC__ ^ (string_of_fun_spec spec)); *)
    (* print_endline (__LOC__ ^ "\n" ^ Env.all_specs env); *)
    (* let pure_to_check = subst_pre_to_check.pure in *)
    (* print_endline (__LOC__ ^ string_of_pred_normal_form subst_pre_to_check); *)

    let post_extv = List.fold_right (@) (List.map fst (subst_pre_to_check.pure)) [] in
    let env = List.fold_left (fun env (v, vty) -> Env.add_vtype env v vty) env post_extv in
    
    let _, pure_to_check = normalize_cond env subst_pre_to_check in
    let spec_to_check = subst_pre_to_check.spec in
    (* print_endline (__LOC__ ^ "\n" ^ Env.all_specs env); *)

    List.iter (fun v -> __LOC__ ^ string_of_fun_spec v |> print_endline) spec_to_check;

  let check_fun_status, env = check_fun env args (List.map fst spec.fvar) spec_to_check (pure_to_check)  in
  (* let check_bool = Sleek.check_pure env (pre_cond) (pure_to_check) in *)
    (* if (check_bool) then *)

      match check_fun_status with
      | Fail -> [] 
      | check_fun_status ->
      let envs = match check_fun_status with
      | Inst vs -> 
        if (List.length vs = 0) then assert false else ();
        List.map (Env.add_inst_group env) vs
      | _ -> [ env ] in

      (let new_anchor = Env.get_fresh_res_name env in
      let new_anchor_ty = snd (fst spec.fpost) in
      let (fpost_anchor, _), fpost_dnf = spec.fpost in
      let fpost_ass = fpost_dnf.pure  in
      let post_subst = subst_ext_pure_pred_list (fpost_anchor) new_anchor fpost_ass in
      let post_spec_subst = List.map (subst_fun_signature (fpost_anchor) new_anchor) fpost_dnf.spec in
      (* TODO: we don't need to substitute program variables here? *)
      List.map (fun env ->
        let post_subst_env = instantiates_pure_preds (SMap.bindings env.fname_assignment) post_subst in
        let env, post_subst_env = normalize_cond env {pure=post_subst_env; spec=[]} in
        let subst_res = subst_pure_preds_list (List.map fst spec.fvar) args post_subst_env in
        Env.add_vtype env new_anchor new_anchor_ty, 
        new_anchor, {
          (* add pre_cond to post_cond and produce the accumulated condition *)
          pure= List.map (fun pred -> [], pred)
            (List.concat_map (add_constraint_pure pre_cond) subst_res );
          spec =post_spec_subst
        }) envs
        
        )
        
    (* else [] *)
    ))
  else 
    (* partial application *)
    let new_name = spec.fname^"'" in
    (* let () = print_endline (string_of_int (List.length spec.fvar)) in 
    let () = print_endline (string_of_int (List.length args)) in *)
    let applied_args, rem_args = 
        split_nth (List.length args) [] spec.fvar in
    let applied_args_names = List.map fst applied_args in 
    [env, new_name, {
      (* pure= List.map (fun (prog_vars, preds) -> (prog_vars, normalize_dnf (snd spec.fspec.fpost).pure preds)) pre_cond ; *)
      pure= List.map (fun v -> [] , v) pre_cond ;
      spec= [{
        fname=new_name; fvar= rem_args; pnames=spec.pnames;
        fpre=subst_pred_normal_forms applied_args_names args spec.fpre;
        fpost=(fst  spec.fpost, subst_pred_normal_forms applied_args_names args (snd spec.fpost));
      }]
    }]


let rec infer_of_expression (env:env) (acc:pred_normal_form) (expr:Parsetree.expression) : 
  (env * logical_var * pred_normal_form) list  =
(* return value: anchor * inferred verification condition *)
let open Parsetree in


  (* I: move the existantial vars & function specifications into the envrionment *)
  let env, pre_cond = normalize_cond env acc in

  (* II: forward execution on res *)
  let res :  (env * logical_var * pred_normal_form)  list=
  match expr.pexp_desc with 
  | Pexp_fun (_, _, {ppat_desc=Ppat_var {txt=arg_name;_};_} (*pattern*), expr) -> 
      infer_of_expression (Env.add_var_name arg_name env) 
        {pure=List.map (fun v -> [],v) pre_cond;spec=[]} expr 
      (* Note:
         we assume that lambda expressions only occurs at the beginning of a let declaration,
         therefore we can safely ignore and proceed the forward verification.
      *)

  | Pexp_construct ({txt=Lident "true";_} , None) ->
      let fresh_anchor = Env.get_fresh_res_name env in
      let new_constraint = Arith (Eq, Pvar fresh_anchor,  Const (Bool true)) in 
      let new_env = Env.add_vtype env fresh_anchor Bool in
      [ (new_env, fresh_anchor, add_constraint acc new_constraint) ]

  | Pexp_construct ({txt=Lident "false";_} , None) ->
      let fresh_anchor = Env.get_fresh_res_name env in
      let new_constraint = Arith (Eq, Pvar fresh_anchor,  Const (Bool false)) in 
      let new_env = Env.add_vtype env fresh_anchor Bool in
      [ (new_env, fresh_anchor, add_constraint acc new_constraint) ]

  | Pexp_constant (Pconst_integer (num, sufix)) ->
      (* for constant x, 
           add constraint _r = x /\ ... *)
      let combined_num_string = match sufix with Some sufix -> String.make 1 sufix | None -> ""in
      let int_val = int_of_string (num ^ combined_num_string) in
      let fresh_anchor = Env.get_fresh_res_name env in
      let new_constraint = Arith (Eq, Pvar fresh_anchor,  Const (Int int_val)) in 
      let new_env = Env.add_vtype env fresh_anchor Int in
      [ (new_env, fresh_anchor, add_constraint acc new_constraint) ]

  | Pexp_ident  {txt=ident;_} -> 
    (* for variable x, just let the anchor to be x, which is equivalent to
         adding constraint _r = x /\ ... *)
    let var_name = string_of_ident ident in
    (* let fresh_name = Env.get_fresh_res_name env in *)
    (* env, fresh_name, subst_pred_normal_form var_name fresh_name acc *)
   (match List.find_opt (String.equal var_name) (env.prog_vars) with 
    | Some _ ->  (
      [( env, var_name, acc ) ])
    | None -> (
      let matched_spec = Env.find_spec var_name env in
      match matched_spec with | None -> failwith ("the identifier " ^ var_name ^ " is not defined") | Some matched_spec ->
      matched_spec |> List.map
      (fun {fvar; fpre; fpost ; _} ->
        if List.length fvar != 0 then ( env, var_name, acc )
        else
          (* the identifier corresponds to a value that have been defined earlier,
             if the pre-condition satisfies, we should further evaluate it
          *)
          (* The argument list is 0, so we can safely check only the pure part, 
            there should be no spec part describing the funtion arguments *)
          (  print_endline __LOC__;
          if Sleek.check_pure env pre_cond fpre.pure then
            let fresh_name = Env.get_fresh_res_name env in
            let fresh_ty = snd (fst fpost)in
            let new_env = Env.add_vtype env var_name fresh_ty in
            let subst_post = subst_pred_normal_form (fst (fst fpost)) fresh_name (snd fpost) in
            ( Env.add_specs_to_fn fresh_name subst_post.spec new_env, fresh_name, { subst_post with spec=[]} )
          else ( env, var_name, acc ))
      )
    ))

  | Pexp_apply (f_exp, arg_list) 
    (* for application: [expression * (arg_label * expression) list]
      split into several steps *) ->
    (* Step 0: evaluate the function *)
    let function_infer = infer_of_expression env acc f_exp in
    
    function_infer |>

    
    
    List.concat_map (fun (env, fname, f_acc) -> 
    (* Step 0: normalize the condition again *)
    let env, pre_cond = normalize_cond env f_acc in

    let arg_expr_list = List.map snd arg_list in
    
    (* Step 1: evaluate all the parameters *)
    let eval_args (expr: expression) (last_res : (env * logical_var list * pred_normal_form) list)
    : (env * logical_var list * pred_normal_form) list =
      (last_res |>
      List.concat_map (fun (old_env, arg_anchors, old_pred) ->
      let infer_res = infer_of_expression old_env old_pred expr in
      infer_res |>
      List.map (fun (new_env, new_anchor, new_cond) -> (new_env, new_anchor::arg_anchors, new_cond)))) in


    let arg_vars = List.fold_right eval_args arg_expr_list [(env, [], {pure=List.map (fun v -> [],v) pre_cond;spec=[]})] in
    arg_vars |> List.concat_map 
      (fun (env, arg_vars, acc) ->

      let env, pre_cond = normalize_cond env acc in

      let fname = fname in
      let fspecs = Env.find_spec fname env in
    
    (* find a function specification in the context that can be used for each disjunction branch *)
      (* TODO: is this combine sound????
      Note: especially for multiple specs?
      *)
    (match fspecs with 
    | None -> failwith ("Function spec not found for [" ^ fname ^ "]")
    | Some fspecs ->
    let valid_fspecs = List.concat_map 
      (check_spec_derive env pre_cond arg_vars) fspecs in
    if (List.length valid_fspecs = 0) then failwith "No valid function spec available" else
      valid_fspecs
      )))
  
  | Pexp_ifthenelse (b_exp, e1, Some e2) ->
      (* Step 0: evaluate the boolean *)
      (infer_of_expression env acc b_exp) |>
      List.concat_map
      (fun (env, bname, f_acc) -> 

      (* Step 0: normalize the condition again *)
      let pre_cond = f_acc.pure in
      let env = List.fold_left 
                  (fun env spec -> Env.add_spec_to_fn spec.fname spec env)
                  env f_acc.spec in 
      let f_acc = {pure=pre_cond; spec=[]} in

      (* Step 1: eval two branches *)
      let true_cond = add_constraint f_acc (Arith (Eq, Pvar bname, Const (Bool true))) in
      (infer_of_expression env true_cond e1) |>
      List.concat_map
      (fun (env, bname_t, f_acc_t) -> 

      let false_cond = add_constraint f_acc (Arith (Eq, Pvar bname, Const (Bool false))) in
      (infer_of_expression env false_cond e2)  |>
      List.concat_map
      (fun (env, bname_f, f_acc_f) ->
      (* TODO: can env be passed consecutively this way? *)

      (* Step 2: unify post_conditions *)
      let unified_anchor = Env.get_fresh_res_name env in
      
      let new_env = 
                  try
                    let unified_anchor_ty = Env.lookup_vtype env bname_t in
                    Env.add_vtype env unified_anchor unified_anchor_ty
                with _ -> env in
      let f_acc_t = subst_pred_normal_form bname_t unified_anchor f_acc_t in
      let f_acc_f = subst_pred_normal_form bname_f unified_anchor f_acc_f in
      [(new_env, unified_anchor, {pure= f_acc_t.pure @ f_acc_f.pure; spec=f_acc_t.spec @ f_acc_f.spec })] )))

  | Pexp_match (exp, case_list) ->
      (* Step 0: evaluate the expression *)
      (infer_of_expression env acc exp) |>
      List.concat_map
      (fun (env, bname, f_acc) -> 

      (* Step 0: normalize the condition again *)
      let pre_cond = f_acc.pure in
      let env = List.fold_left 
                  (fun env spec -> Env.add_spec_to_fn spec.fname spec env)
                  env f_acc.spec in 
      let f_acc = {pure=pre_cond; spec=[]} in

      let fresh_anchor = Env.get_fresh_res_name env in

      (* Step 1: add case constraint *)
      List.fold_left
      (fun last_res {pc_lhs; pc_rhs; _} ->
        last_res |> List.concat_map
        (fun (env, last_anchor, last_cond) ->
          match pc_lhs.ppat_desc with
          | Ppat_construct (pat_name, pat_args) ->
              let pat_name = string_of_ident pat_name.txt in
              let pat_args_name = (match pat_args with None -> []
              | Some {ppat_desc=(Ppat_tuple args);_} -> List.map (
                  fun pat_arg -> match pat_arg.ppat_desc with
                  | Ppat_var x -> (x.txt)
                  | _ -> failwith "Other pattern argument not available"
              ) args
              | _ -> failwith "Other pattern structure not available") in
              let pat_args_ty = Env.lookup_constr_arg_ty env pat_name in
              let pat_args_pairs = List.combine pat_args_name pat_args_ty in
              let new_constraint =  Field (bname, pat_name, List.map (fun v -> Lvar v) pat_args_name) in
              (* add global program variable *)
              let new_env = List.fold_left (fun old_env v -> Env.add_var_name v old_env) env pat_args_name in
              (* add type information *)
              let new_env = List.fold_left (fun old_env (v, vty) -> Env.add_vtype old_env v vty) new_env pat_args_pairs in
              let inferred_cond = add_constraint f_acc new_constraint in
              infer_of_expression new_env inferred_cond pc_rhs |>
              List.map (fun (env', anchor', acc') ->
                (* at least one solution, so we can complete the type information of unified anchor here *)
                  let env' = 
                  try
                    let unified_anchor_ty = Env.lookup_vtype env' anchor' in
                    Env.add_vtype env' last_anchor unified_anchor_ty
                with _ -> env' in
                
                let subst_acc = subst_pred_normal_form anchor' last_anchor acc' in
                (env', last_anchor,
                  {pure=last_cond.pure @ subst_acc.pure;
                   spec=last_cond.spec @ subst_acc.spec }))
          | _ -> failwith "Other pattern matching not available"
      )) [(env, fresh_anchor, {pure=[]; spec=[]})]  case_list )
  
  | _ -> assert false
    in 
    List.iteri
    (fun i res -> 
        print_endline ("hip[" ^ string_of_int i ^"]: " ^ 
            match res with env, anchor, res ->  
              "[" ^ (String.concat "," (env.prog_vars)) ^ "]" ^
              anchor ^ " : " ^  string_of_pred_normal_form (res))) res; res

(* Check for each let function declaration *)
let infer_of_value_binding env (val_binding:Parsetree.value_binding) 
(* : string * env  *)
= 
  let fn_name = string_of_pattern val_binding.pvb_pat.ppat_desc in
  let body = val_binding.pvb_expr in
  (* let formals = collect_program_vars body in *)
  let spec = match (Env.find_spec fn_name env) with
            | Some (spec::_) -> spec
            (* TODO: verify all specs *)
            | _ -> failwith ("not enough spec for " ^ fn_name) in
    (* match function_spec body with
    | None -> default_spec
    | Some (pre, post) -> (normalSpec pre, normalSpec post) *)
  let env = List.fold_left (fun old_env (vname, vty) -> Env.add_vtype old_env vname vty) env spec.fvar in
  infer_of_expression env spec.fpre body |>
  (
  List.fold_left (fun last_res (inferred_env, post_anchor, inferred_post) ->
    (* normalize specs *)
    let inferred_env = 
      Env.add_specs_to_fn post_anchor inferred_post.spec inferred_env in
    let inferred_post = { inferred_post with spec = [] } in

    (if last_res then true else
    (let substed_post = subst_pred_normal_form (fst (fst spec.fpost)) post_anchor (snd spec.fpost) in

    (* TODO: spec should also be substed if the return value is a function specification  *)

    let post_pure = ( substed_post.pure ) in
    let post_spec = ( substed_post.spec ) in

    let pure_env, inferred_post_pure = normalize_cond inferred_env inferred_post in

    (* print_endline __LOC__; *)

    let pure_check = Sleek.check_pure pure_env ( inferred_post_pure ) post_pure in

    let check_post_spec (expect_spec:fun_signature) : bool = 
      (let exist_spec = Env.find_spec expect_spec.fname inferred_env in
      (match exist_spec with 
      |  None -> 
        print_endline (Env.all_specs env);
        failwith ("No spec exist for expected return spec named " ^ expect_spec.fname)
      | Some exist_spec ->
          (let valid_exist_spec = List.filter 
              (fun spec -> match Sleek.check_spec_sub inferred_env inferred_post_pure expect_spec spec with
               | Success -> true | _ -> false
              ) exist_spec in
            List.length valid_exist_spec != 0))) in

    let spec_check = List.length (List.filter check_post_spec post_spec) = List.length post_spec in 
      if (pure_check && spec_check) then true else
        (false))
  )) false
  )
  (* let inferred_env, post_anchor, inferred_post = infer_of_expression env spec.fpre body in
    let _ = fn_name, formals, inferred_post, post_anchor, inferred_env in
    let normalized_post = ( (snd spec.fpost).pure ) in
    let substed_post = subst_pure_pred_list (fst spec.fpost) (Env.get_top_res_name env) normalized_post in
      if (Sleek.check_pure ( inferred_post.pure ) substed_post) then true else
        (
          (* print_endline "-------------------";
        Env.print_all_insts inferred_env;
        print_endline "-------------------"; *)
        (* if (Sleek.check_pure ( inferred_post.pure ) substed_post) then true else *)
        false) *)



let infer_of_program env (prog:Parsetree.structure_item)
(* : string * env  *)
=
  match prog.pstr_desc with
  | Pstr_value (_ (*rec_flag*), x::_ (*value_binding list*)) ->
    (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
       let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
     *)
      infer_of_value_binding env x 
  (* | _ ->  pure_pred_to_string False, env *)
  | _ -> assert false
  ;;

let name_of_prog (prog: Parsetree.structure_item) : string =
  match prog.pstr_desc with 
  | Pstr_value (_ (*rec_flag*), x::_ (*value_binding list*)) ->
    string_of_pattern x.pvb_pat.ppat_desc
  | _ -> assert false
  ;;

(* 
let hip_main () =
  let inputfile = 
    try (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) with
    e -> let _ = e in failwith "need to input a file name" in
(*    let outputfile = (Sys.getcwd ()^ "/" ^ Sys.argv.(2)) in
print_string (inputfile ^ "\n" ^ outputfile^"\n");*)
  let ic = open_in inputfile in
  try
    let lines =  (input_lines ic ) in
    let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (List.rev lines) "" in
    
    (* debug_tokens line; *)
    (* print_string line; *)

    let progs = Parser.implementation Lexer.token (Lexing.from_string line) in


    (* Dump AST -dparsetree-style *)
    Format.printf "%a@." Printast.implementation progs;

    (* let results, _ =
    List.fold_left (fun (s, env) a ->
      let spec, env1 = infer_of_program env a in
      spec :: s, env1
    ) ([], Env.empty) progs
    in
    print_endline (results |> List.rev |> String.concat "\n"); *)

    (*print_string (Pprintast.string_of_structure progs ) ; *)
    (* print_string (List.fold_left (fun acc a -> acc ^ string_of_program a) "" progs); *)

    (* print_string (List.fold_left (fun acc a -> acc ^ (infer_of_program progs a) ^ "\n" ) "\n" progs); *)

    (*print_endline (Pprintast.string_of_structure progs ) ; 
    print_endline ("---");
    print_endline (List.fold_left (fun acc a -> acc ^ forward a) "" progs);*)
    flush stdout;                (* ???????????????????????? *)
    close_in ic                  (* ?????????????????? *)

  with e ->                      (* ????????????????????????????????? *)
    flush stdout;                (* ???????????????????????? *)
    close_in_noerr ic;           (* ???????????? *)
    raise e                      (* ????????????????????????: ???????????????,??????????????????????????? *)
   ;; *)