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
}

let digit = ['0'-'9']
let int = '-'? digit digit*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white  { read lexbuf }  (* skip blanks *)
  | newline { read lexbuf }
  | "//" { comment lexbuf }
  | '{'    { LEFT_BRACKET }
  | '}'    { RIGHT_BRACKET }
  | '['    { LEFT_SQUARE }
  | ']'    { RIGHT_SQUARE }
  | ','    { COMMA }
  | int    { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "true" { TRUE }
  | ')'    { RIGHT_BRACE }
  | "+"    { PLUS }
  | "||"   { OR }
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
  | id     { ID (Lexing.lexeme lexbuf) }
  | _      { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof    { EOF }
and comment =
  parse
  | newline { read lexbuf }
  | eof     { EOF }
  | _       { comment lexbuf }