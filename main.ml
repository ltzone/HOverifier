let make_sig sigs predicates pty_env =
  let open Hiphop in
  let with_sig =
  List.fold_right (fun (si:Spectree.fun_signature) -> Spectree.Env.add_spec_to_fn si.fname si ) sigs Spectree.Env.empty in
  { with_sig with
      predicates = predicates;
      ftype_context = pty_env;
  }

let double_env = 
  let open Specs.Double in
  make_sig sigs predicates pty_env

let div_env = 
  let open Specs.Div in
  make_sig sigs predicates pty_env

(* let trivial_env = 
  let open Specs.Trivial in
  make_sig sigs *)

let test () = 
  let open Hiphop.Hip in
  (* let inputfile = "testcases/div.ml" in *)
  let inputfile = "testcases/double.ml" in
  (* let inputfile = "testcases/trivial.ml" in *)
  let ic = open_in inputfile in
  let output = ref "\nResult:" in
  try
    let lines =  (input_lines ic ) in
    let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (List.rev lines) "" in
    

    let progs = Parser.implementation Lexer.token (Lexing.from_string line) in
    (* let env = div_env in *)
    let env = double_env in
    (* let env = trivial_env in *)
    
    List.iter (fun prog -> (  

      let fname = (name_of_prog prog) in
      Format.printf "Verifying %s\n" fname;
    (* Format.printf "%a@." Printast.implementation [prog]; *)
        if infer_of_program env prog 
        then
          (output := !output ^ "\n " ^ fname ^ " verified";
          Format.printf "Verify %s success\n" fname)
        else 
          (output := !output ^ "\n " ^ fname ^ " failed";
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