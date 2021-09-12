
module Parser = Frontend.Parser
module Lexer = Frontend.Lexer
module Printast = Frontend.Printast

let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (String.trim line) :: input_lines file
  | _ -> failwith "Weird input_line return value"





let () =
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