{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }



type state = CODE | SPEC
let state = ref CODE
}

let digit = ['0'-'9']
let int = '-'? digit digit*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*


rule read_code = 
  parse
  | "(*@"   { state := SPEC; read lexbuf }
  | eof     { EOF }
  | newline {  next_line lexbuf; read_code lexbuf }
  | _       { 
    (* print_string (Lexing.lexeme lexbuf);  *)
    read_code lexbuf }
and read =
  parse
  | white  { read lexbuf }  (* skip blanks *)
  | newline {  next_line lexbuf; read lexbuf }
  | "//"   { comment lexbuf }
  | "@*)"   { state := CODE; read_code lexbuf }
  | '{'    { LEFT_BRACKET }
  | '}'    { RIGHT_BRACKET }
  | '['    { LEFT_SQUARE }
  | ']'    { RIGHT_SQUARE }
  | ','    { COMMA }
  | int    { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "true" { TRUE }
  | ')'    { RIGHT_BRACE }
  | "+"    { PLUS }
  | "&"    { AND }
  | "||"   { OR }
  | "or"   { OR }
  | "*"    { MULT }
  | "-"    { MINUS }
  | "("    { LEFT_BRACE }
  | "<="   { LE }
  | "false" { FALSE }
  | "requires" { REQUIRES }
  | "ensures" { ENSURES }
  | "declare" { DECLARE }
  | "pred" { PRED }
  | "="   { EQ }
  | ","     { COLON }
  | "int"     { TINT }
  | "bool"     { TBOOL }
  | "|="        { HASSPEC }
  | "*->:"      { PREPOST }
  | "with"      { WITH }
  | "~"         { NEG }
  | ":"         { COLON }
  | "given"     { GIVEN }
  | id     { ID (Lexing.lexeme lexbuf) }
  | _      { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof    { EOF }
and comment =
  parse
  | newline {  next_line lexbuf; read lexbuf }
  | eof     { EOF }
  | _       { comment lexbuf }


{
  let lexer lb =
    match !state with
      CODE -> read_code lb
    | SPEC -> read lb
}