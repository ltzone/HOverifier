let make_sig sigs predicates pty_env =
  let open Hiphop in
  let with_sig =
  List.fold_right (fun (si:Spectree.fun_signature) -> Spectree.Env.add_spec_to_fn si.fname si ) sigs Spectree.Env.empty in
  { with_sig with
      predicates = predicates;
      ftype_context = pty_env;
  }

let make_sig_from_file filename = 
  let open Specparser.Driver in
  let open Hiphop.Spectree in
  let sigs, predicates, pty_env = parse_from_file filename in
  print_endline (string_of_parse (parse_from_file filename));
  (* print_endline (__LOC__ ^ "\n"^ string_of_int (List.length (Hiphop.Spectree.SMap.bindings pty_env))); *)
  let sigs = Specparser.Prelude.sigs @ sigs in
  let predicates = Specparser.Prelude.predicates @ predicates in

  let make_pty_env_sigle ({ pname; pargs; _}) pty_map =
    SMap.add pname  (List.map snd pargs) pty_map in
  let pty_env = List.fold_right make_pty_env_sigle predicates pty_env in
  make_sig sigs predicates pty_env

let test () = 
  let open Hiphop.Hip in
  let open Frontend.Parsetree in
  let module_name = 
    if Array.length (Sys.argv) <= 1 then
      "double"
    else Array.get Sys.argv 1 in
  let input_module = "testcases/" ^ module_name in
  let input_program = input_module ^ ".ml" in
  let input_spec = input_module ^ ".ml" in
  let env = make_sig_from_file input_spec in
  (* print_endline (__LOC__ ^ string_of_int (List.length (Hiphop.Spectree.SMap.bindings env.ftype_context))); *)
  let ic = open_in input_program in
  print_endline ("[TEST] " ^ module_name);
  let output = ref "\nResult:" in
  try
    let lines =  (input_lines ic ) in
    let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (List.rev lines) "" in
    

    let progs = Parser.implementation Lexer.token (Lexing.from_string line) in

    let prog_tys = List.concat (List.filter_map (fun prog ->
      match prog.pstr_desc with
      | Pstr_type (_, dls) -> Some dls
      | _ -> None
      ) progs) in
    let env = List.fold_left (Hiphop.Spectree.Env.add_adt_decl) env prog_tys in
    (* let ctx  = Z3.mk_context [] in *)
    (* print_endline __LOC__;
    print_endline (Hiphop.Spectree.Env.all_adts env ctx); *)

    let progs = List.filter_map (fun prog ->
      match prog.pstr_desc with
      | Pstr_value _ -> Some prog
      | _ -> None
      ) progs in
    
    List.iter (fun prog -> (  

      let fname = (name_of_prog prog) in
      Format.printf "Verifying %s\n" fname;
    (* Format.printf "%a@." Printast.implementation [prog]; *)
      try
        if infer_of_program env prog 
        then
          (output := !output ^ "\n " ^ fname ^ " verified";
          Format.printf "Verify %s success\n" fname)
        else 
          (output := !output ^ "\n " ^ fname ^ " failed";
          Format.printf "Verify %s fail\n" fname);
          print_endline "\n------------------------\n"
        with
      Failure e -> (output := !output ^ "\n " ^ fname ^ " failed because " ^ e;
          Format.printf "Verify %s fail\n" fname);
          print_endline "\n------------------------\n"
        )) progs ;

    flush stdout;                (* 现在写入默认设备 *)
    close_in ic                  (* 关闭输入通道 *);
    print_endline !output;
    
  with e ->                      (* 一些不可预见的异常发生 *)
    close_in_noerr ic;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
   ;;





(* let () = Test_mlapi.main () *)
let _ = 
  (* print_endline "-------------- SLEEK MAIN ----------------"; *)
  (* Hiphop.Sleek.main (); *)
  (* print_endline "--------------- HIP MAIN -----------------"; *)
  test ()


(*

TODO:

When calling another function (using its spec),

if the free variables in the current assertions coincide 
with the variables used in the specification to be used,
the binder variables of the specification should be first alpha-renamed

*)