open Lexing

let string_of_parse p =
  let open Hiphop.Spectree in
  let specs, preds, _ = p in
  String.concat "\n" (List.map string_of_fun_spec specs) ^ "\n" ^
  String.concat "\n" (List.map logical_proposition_to_string preds)

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Core.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.parse Lexer.lexer lexbuf with
  | Lexer.SyntaxError msg ->
    Core.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    [],[], Hiphop.Spectree.SMap.empty
  | Parser.Error ->
    print_endline (Lexing.lexeme lexbuf);
    Core.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    [],[], Hiphop.Spectree.SMap.empty
    (* exit (-1) *)


let parse_from_file filename =
  let inx = Core.In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let res = parse_with_error lexbuf in
  Core.In_channel.close inx; res
    

(* let _ = print_endline (string_of_parse (parse_from_file "./testcases/div.ml"));; *)