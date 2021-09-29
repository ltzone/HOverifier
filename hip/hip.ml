
module Parser = Frontend.Parser
module Lexer = Frontend.Lexer
module Printast = Frontend.Printast
module Parsetree = Frontend.Parsetree
open Spectree

let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (String.trim line) :: input_lines file
  | _ -> failwith "Weird input_line return value"

module SMap = Map.Make (struct
  type t = string
  let compare = compare
end)

module SSet = Set.Make(String)

type env = {
  (* module name -> a bunch of function specs *)
  specs : fun_signature list SMap.t;
  (* fnames : SSet.t; *)
}


module Env = struct
  let empty = {
    specs = SMap.empty
  }

  let add_fn fname specs env =
    { specs = SMap.add fname specs env.specs; }
  
  let add_spec_to_fn fname spec env = 
    { specs = SMap.update fname (function None -> Some [spec]
                                | Some specs -> Some (spec::specs)) env.specs }

  let find_spec fname env = SMap.find_opt fname env.specs

end


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


let normalize_dnf (a: (program_var list * pure_pred) list) : pure_pred = 
  List.fold_right (fun v vs -> (Or (snd v, vs))) a False

(* TODO: first check partial/full application, then check pre-post SMT *)
let check_spec_derive env pre_cond args (spec:fun_signature)  : pred_normal_form option =
  let _ = env in
  let pred_to_check = spec.fspec.fpre in
  let check_bool = Sleek.check_pure (normalize_dnf pre_cond) (normalize_dnf pred_to_check.pure) in
  if check_bool then 
    (if List.length args = List.length spec.fvar then
      (* partial application *)
    Some {
      pure= List.map (fun (prog_vars, preds) -> (prog_vars, And (preds, normalize_dnf (snd spec.fspec.fpost).pure))) pre_cond ;
      spec= []
    } else 
      (* full application *)
    Some {
      pure= List.map (fun (prog_vars, preds) -> (prog_vars, And (preds, normalize_dnf (snd spec.fspec.fpost).pure))) pre_cond ;
      spec= [{
        fname=spec.fname^"'"; fvar= List.tl spec.fvar; fspec=spec.fspec 
      }]
    })
  else None

let rec infer_of_expression (env:env) (acc:pred_normal_form) (expr:Parsetree.expression) : pred_normal_form  =
  let open Parsetree in
  let pre_cond = acc.pure in
  let env = List.fold_left 
              (fun env spec -> Env.add_spec_to_fn spec.fname spec env)
              env acc.spec in  
  (* move the function specifications into the envrionment *)
  match expr.pexp_desc with 
  | Pexp_fun (_, _, _ (*pattern*), expr) -> 
      infer_of_expression env acc expr 
      (* Note:
         we assume trat lambda expressions only occurs at the beginning of a let declaration,
         therefore we can safely ignore and proceed the forward verification.
      *)

  | Pexp_apply ({pexp_desc=Pexp_ident {txt=fname;_} ;_ }, arg_list) (*expression * (arg_label * expression) list*) ->
    let fname = Frontend.Longident.last fname in
    let fspecs = Env.find_spec fname env in

    let arg_vars = List.map (fun (_, arg_exp) -> match arg_exp.pexp_desc with 
                                                  Pexp_ident {txt=argname;_} -> Frontend.Longident.last argname 
                                                  | _ -> assert false ) arg_list in

    (* find a function specification in the context that can be used for each disjunction branch *)
    (match fspecs with None -> failwith "Function spec not found" | Some fspecs ->
    let valid_fspecs = List.filter_map 
      (check_spec_derive env pre_cond arg_vars) fspecs in
    let combine_fspecs {pure; spec} {pure=old_pure; spec=old_spec} = {pure=pure@old_pure;spec=spec@old_spec} in
      List.fold_left combine_fspecs {pure=[];spec=[]} valid_fspecs)
  | _ -> assert false


(* Check for each let function declaration *)
let infer_of_value_binding env (val_binding:Parsetree.value_binding) 
(* : string * env  *)
= 
  let fn_name = string_of_pattern val_binding.pvb_pat.ppat_desc in
  let body = val_binding.pvb_expr in
  let formals = collect_program_vars body in
  let spec = match (Env.find_spec fn_name env) with
            | Some (spec::_) -> spec
            | _ -> failwith ("not enough spec for " ^ fn_name) in
  let spec = spec.fspec in
    (* match function_spec body with
    | None -> default_spec
    | Some (pre, post) -> (normalSpec pre, normalSpec post) *)
  let inferred_post = infer_of_expression env spec.fpre body in
    let _ = fn_name, formals, inferred_post in
inferred_post



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

let test () = 
  let inputfile = "testcases/t0_twice.ml" in
  let ic = open_in inputfile in
  try
    let lines =  (input_lines ic ) in
    let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (List.rev lines) "" in
    

    let progs = Parser.implementation Lexer.token (Lexing.from_string line) in
    let prog = List.nth progs 0 in
    Format.printf "%a@." Printast.implementation [prog];
    let env = Env.empty |> (Env.add_spec_to_fn "once" once_sig) in

    let res = infer_of_program env prog in
    flush stdout;                (* 现在写入默认设备 *)
    close_in ic                  (* 关闭输入通道 *);
    print_endline "success verify"; res
  with e ->                      (* 一些不可预见的异常发生 *)
    close_in_noerr ic;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
   ;;


let hip_main () =
  let inputfile = (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) in
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
    close_in_noerr ic;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
   ;;