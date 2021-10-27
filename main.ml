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
  (* let open Hiphop.Spectree in *)
  let sigs, predicates, pty_env = parse_from_file filename in
  print_endline (string_of_parse (parse_from_file filename));
  let sigs = Specparser.Prelude.sigs @ sigs in
  let predicates = Specparser.Prelude.predicates @ predicates in

  (* let make_pty_env_sigle ({ pname; pargs; _}: logical_proposition) pty_map =
    SMap.add pname  (List.map snd pargs) pty_map in
  let pty_env = List.fold_right make_pty_env_sigle predicates SMap.empty in *)
  make_sig sigs predicates pty_env

let test () = 
  let open Hiphop.Hip in
  let module_name = 
    if Array.length (Sys.argv) < 1 then
      "double"
    else Array.get Sys.argv 1 in
  let input_module = "testcases/" ^ module_name in
  let input_program = input_module ^ ".ml" in
  let input_spec = input_module ^ ".ss" in
  let env = make_sig_from_file input_spec in
  let ic = open_in input_program in
  let output = ref "\nResult:" in
  try
    let lines =  (input_lines ic ) in
    let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (List.rev lines) "" in
    

    let progs = Parser.implementation Lexer.token (Lexing.from_string line) in
    
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