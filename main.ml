
let trivial_env =
  let open Specs.Trivial in
  let open Hiphop.Hip in
  Env.empty |> (Env.add_spec_to_fn "once" once_sig) 
  |> (Env.add_spec_to_fn "two_arg" two_arg_sig)
  |> (Env.add_spec_to_fn "twice" twice_sig) 

let make_sig sigs =
  let open Hiphop.Hip in
  let open Hiphop in
  List.fold_right (fun (si:Spectree.fun_signature) -> Env.add_spec_to_fn si.fname si ) sigs Env.empty

let double_env = 
  let open Specs.Double in
  make_sig sigs



let test () = 
  let open Hiphop.Hip in
  let inputfile = "testcases/double.ml" in
  let ic = open_in inputfile in
  try
    let lines =  (input_lines ic ) in
    let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (List.rev lines) "" in
    

    let progs = Parser.implementation Lexer.token (Lexing.from_string line) in
    let env = double_env in
    
    List.iter (fun prog -> (  Format.printf "Verifying %s\n" (name_of_prog prog);
    (* Format.printf "%a@." Printast.implementation [prog]; *)
                              if infer_of_program env prog 
                              then Format.printf "Verify %s success\n" (name_of_prog prog)
                              else Format.printf "Verify %s fail\n" (name_of_prog prog));Format.printf "\n\n\n\n"; flush stdout) progs ;

    flush stdout;                (* 现在写入默认设备 *)
    close_in ic                  (* 关闭输入通道 *);
    
  with e ->                      (* 一些不可预见的异常发生 *)
    close_in_noerr ic;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
   ;;





(* let () = Test_mlapi.main () *)
let _ = 
  (* print_endline "-------------- SLEEK MAIN ----------------";
  Sleek.main ();
  print_endline "--------------- HIP MAIN -----------------"; *)
  test ()