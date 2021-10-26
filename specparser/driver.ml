let file = "./specparser/test.txt"

let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (String.trim line) :: input_lines file
  | _ -> failwith "Weird input_line return value"

let string_of_parse p =
  let open Hiphop.Spectree in
  let specs, preds = p in
  String.concat "\n" (List.map string_of_fun_spec specs) ^ "\n" ^
  String.concat "\n" (List.map logical_proposition_to_string preds)

let test () = 
  let ic = open_in file in
  try
    let lines = (input_lines ic ) in
    let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (List.rev lines) "" in

    let res = Parser.parse Lexer.read (Lexing.from_string line) in


    flush stdout;                (* 现在写入默认设备 *)
    close_in ic                  (* 关闭输入通道 *);
    res
    
  with e ->                      (* 一些不可预见的异常发生 *)
    close_in_noerr ic;           (* 紧急关闭 *)
    raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)
   ;;

let _ = print_endline (string_of_parse ((test ())));;