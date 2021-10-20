
module Parser = Frontend.Parser
module Lexer = Frontend.Lexer
module Printast = Frontend.Printast
module Parsetree = Frontend.Parsetree
open Spectree

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


let check_fun env args fvars spec_to_check pre : spec_res * env =
  let find_spec_name fname =
    print_endline ("11111111111");
    print_endline (String.concat "," fvars);
    print_endline (String.concat "," args);
    findi_opt (String.equal fname) fvars in
  let check_single_fun_spec spec =
    match (find_spec_name spec.fname) with
    | None -> failwith ("The specification declared as " ^ spec.fname ^ " is not part of the argument list")
    | Some i ->
        let fun_name_in_env = List.nth args i in
        let client_spec = Env.find_spec fun_name_in_env env in
        match client_spec with
        (* TODO: try multiple specifications *)
          | Some (client_spec :: _) -> Sleek.check_spec_sub env pre client_spec spec
          | _ -> failwith ("No specification found for function " ^ fun_name_in_env)
    in
    (* print_endline ("Spec to check:::::" ^ string_of_int (List.length spec_to_check)); *)
  print_length (spec_to_check);
  let res = List.fold_right (
    fun spec (bres, (env:env)) -> match bres with Fail -> Fail, env
        | _ -> (match (check_single_fun_spec spec) with
          | Fail -> Fail, env
          | Inst vs -> (Inst vs, List.fold_right (fun v env -> 
            print_endline ("adding inst for " ^ fst v);
            print_endline (logical_proposition_to_string (snd v));
            Env.add_inst env (fst v) (snd v)) vs env)
          | Success -> Success, env )  (* TODO: mark the instantiated? *)) spec_to_check (Success, env) in res


  (* TODO: first check partial/full application, then check pre-post SMT *)
let check_spec_derive env pre_cond args (spec:fun_signature)  : (logical_var * pred_normal_form) option =
(* let spec_to_check = spec.fspec.fpre.spec in *)
(* let check_env = check_spec env spec_to_check in *)
(* if check_bool && check_env then  *)
  if List.length args = List.length spec.fvar then
    (* full application *)
    (let subst_pre_to_check = subst_pred_normal_forms spec.fvar args spec.fpre in
    let pure_to_check = subst_pre_to_check.pure in
    let spec_to_check = subst_pre_to_check.spec in
    (* print_endline "00000000000000";
    (* List.iter (fun v -> print_endline v) args; *)
    List.iter (fun v -> print_endline (string_of_fun_spec v)) spec_to_check;
    (* List.iter (fun v -> print_endline (string_of_fun_spec v)) spec.fpre.spec; *)
    print_endline "00000000000000"; *)
  let check_env, env = check_fun env args spec.fvar spec_to_check pure_to_check  in
  let check_bool = Sleek.check_pure (pre_cond) (pure_to_check) in
    if (check_bool) then
      match check_env with
      | Fail -> None 
      | _ ->
      (let new_anchor = Env.get_fresh_res_name env in
      Some (new_anchor, {
        pure= List.concat_map (fun (preds) -> (
          let fpost_anchor, fpost_dnf = spec.fpost in
          let fpost_ass = preds_and_pred fpost_dnf.pure preds in
          let post_subst = subst_pure_pred_list fpost_anchor new_anchor fpost_ass in
          let post_subst_env = instantiate_pure_preds env post_subst in
          let conj_res = preds_and_pred post_subst_env preds in
          let subst_res = subst_pure_preds_list spec.fvar args conj_res in
          (* let subst_res = And (preds, post_subst) in *)
          (* print_endline "----------->";
          print_endline (pure_pred_to_string post_subst);
          print_endline (String.concat "," spec.fvar);
          print_endline (String.concat "," args);
          print_endline (pure_pred_to_string preds);
          print_endline (pure_pred_to_string subst_res);
          print_endline "<-----------"; *)
          subst_res)
        ) pre_cond ;
        spec= []
        }))
        
    else None)
  else 
    (* partial application *)
    let new_name = spec.fname^"'" in
    (* let () = print_endline (string_of_int (List.length spec.fvar)) in 
    let () = print_endline (string_of_int (List.length args)) in *)
    let applied_args, rem_args = 
        split_nth (List.length args) [] spec.fvar in
    Some (new_name, {
      (* pure= List.map (fun (prog_vars, preds) -> (prog_vars, normalize_dnf (snd spec.fspec.fpost).pure preds)) pre_cond ; *)
      pure= pre_cond ;
      spec= [{
        fname=new_name; fvar= rem_args; pnames=spec.pnames;
        fpre=subst_pred_normal_forms applied_args args spec.fpre;
        fpost=(fst  spec.fpost, subst_pred_normal_forms applied_args args (snd spec.fpost));
      }]
    })

let add_constraint (acc: pred_normal_form) (pred: pure_pred) : pred_normal_form =
  { acc with pure= List.map (fun preds -> And (pred, preds)) (acc.pure) }


let rec infer_of_expression (env:env) (acc:pred_normal_form) (expr:Parsetree.expression) : env * logical_var * pred_normal_form  =
(* return value: anchor * inferred verification condition *)
  (* I: move the function specifications into the envrionment *)
  let open Parsetree in
  let pre_cond = acc.pure in
  let env = List.fold_left 
              (fun env spec -> Env.add_spec_to_fn spec.fname spec env)
              env acc.spec in  
  let acc = {pure=pre_cond; spec=[]} in

  (* II: forward execution on res *)
  let res :  env * logical_var * pred_normal_form =
  match expr.pexp_desc with 
  | Pexp_fun (_, _, _ (*pattern*), expr) -> 
      infer_of_expression env acc expr 
      (* Note:
         we assume that lambda expressions only occurs at the beginning of a let declaration,
         therefore we can safely ignore and proceed the forward verification.
      *)

  | Pexp_constant (Pconst_integer (num, sufix)) ->
      (* for constant x, 
           add constraint _r = x /\ ... *)
      let combined_num_string = match sufix with Some sufix -> String.make 1 sufix | None -> ""in
      let int_val = int_of_string (num ^ combined_num_string) in
      let fresh_anchor = Env.get_fresh_res_name env in
      let new_constraint = Arith (Eq, Pvar fresh_anchor,  Const (Int int_val)) in 
      env, fresh_anchor, add_constraint acc new_constraint

  | Pexp_ident  {txt=ident;_} -> 
    (* for variable x, just let the anchor to be x, which is equivalent to
         adding constraint _r = x /\ ... *)
    let var_name = string_of_ident ident in
    (* let fresh_name = Env.get_fresh_res_name env in *)
    (* env, fresh_name, subst_pred_normal_form var_name fresh_name acc *)
    env, var_name, acc

  | Pexp_apply (f_exp, arg_list) 
    (* for application: [expression * (arg_label * expression) list]
      split into several steps *) ->
    (* Step 0: evaluate the function *)
    let env, fname, f_acc = 
      infer_of_expression env acc f_exp in



    (* Step 0: normalize the condition again *)
    let pre_cond = f_acc.pure in
    let env = List.fold_left 
                (fun env spec -> Env.add_spec_to_fn spec.fname spec env)
                env f_acc.spec in  
    let f_acc = {pure=pre_cond; spec=[]} in
    print_endline (String.concat "," (Env.available_names env));

    let arg_expr_list = List.map snd arg_list in
    
    (* Step 1: evaluate all the parameters *)
    let eval_args (expr: expression) (last_res : env * logical_var list * pred_normal_form)
    : env * logical_var list * pred_normal_form =
    let old_env, arg_anchors, old_pred = last_res in
    let new_env, new_anchor, new_cond = infer_of_expression old_env old_pred expr in
    (new_env, new_anchor::arg_anchors, new_cond) in
    let arg_vars = List.fold_right eval_args arg_expr_list (env, [], f_acc) in
    let env, arg_vars, acc = arg_vars in

    let fname = fname in
    let fspecs = Env.find_spec fname env in
    
    (* find a function specification in the context that can be used for each disjunction branch *)
    (match fspecs with 
    | None -> failwith ("Function spec not found for [" ^ fname ^ "]")
    | Some fspecs ->
    let valid_fspecs = List.filter_map 
      (check_spec_derive env acc.pure arg_vars) fspecs in
    if (List.length valid_fspecs = 0) then failwith "No valid function spec available" else
    let unified_anchor = Env.get_fresh_res_name env in
    let combine_fspecs {pure=old_pure; spec=old_spec} (anchor, {pure; spec}) = 
      { pure= (List.map (fun (pure) -> (subst_pure_pred anchor unified_anchor pure)) pure) @ old_pure;
        spec= (List.map ( fun v ->
          print_endline "0000000000000";
          print_endline (string_of_fun_spec v);
          print_endline anchor; print_endline unified_anchor;
          print_endline (string_of_fun_spec (subst_fun_signature_name anchor unified_anchor v));
          subst_fun_signature_name anchor unified_anchor v) spec)@old_spec (* TODO: subst specification *)
      } in
      (env, unified_anchor, List.fold_left combine_fspecs {pure=[];spec=[]} valid_fspecs))
  | _ -> assert false

    in print_endline ("hip: " ^match res with _, anchor, res ->  anchor ^ " : " ^  string_of_pred_normal_form (res)); res

(* Check for each let function declaration *)
let infer_of_value_binding env (val_binding:Parsetree.value_binding) 
(* : string * env  *)
= 
  let fn_name = string_of_pattern val_binding.pvb_pat.ppat_desc in
  let body = val_binding.pvb_expr in
  let formals = collect_program_vars body in
  let spec = match (Env.find_spec fn_name env) with
            | Some (spec::_) -> spec
            (* TODO: verify all specs *)
            | _ -> failwith ("not enough spec for " ^ fn_name) in
    (* match function_spec body with
    | None -> default_spec
    | Some (pre, post) -> (normalSpec pre, normalSpec post) *)
  let inferred_env, post_anchor, inferred_post = infer_of_expression env spec.fpre body in
    let _ = fn_name, formals, inferred_post, post_anchor, inferred_env in
    let normalized_post = ( (snd spec.fpost).pure ) in
    let substed_post = subst_pure_pred_list (fst spec.fpost) (Env.get_top_res_name env) normalized_post in
      if (Sleek.check_pure ( inferred_post.pure ) substed_post) then true else false



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
    flush stdout;                (* 现在写入默认设备 *)
    close_in ic                  (* 关闭输入通道 *)

  with e ->                      (* 一些不可预见的异常发生 *)
    flush stdout;                (* 现在写入默认设备 *)
    close_in_noerr ic;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
   ;; *)