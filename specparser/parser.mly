%{ 
open Hiphop.Spectree;;
%}

%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_SQUARE
%token RIGHT_SQUARE
%token PLUS MINUS MULT DIV 
%token AND OR
%token COMMA
%token EQ
%token LE
%token EOF
%token REQUIRES
%token ENSURES
%token PRED
%token COLON
%token DECLARE
%token HASSPEC PREPOST WITH
%token TINT TBOOL
%token NEG
%token GIVEN

%start <Hiphop.Spectree.fun_signature list * 
        Hiphop.Spectree.logical_proposition list * 
        Hiphop.Spectree.exp_type list Hiphop.Spectree.SMap.t> parse

%nonassoc NEG
%nonassoc EQ LE
%left AND OR
%left PLUS MINUS
%left MULT DIV
%nonassoc LEFT_BRACE
%nonassoc ID
%nonassoc TRUE FALSE INT
%%

let parse :=
| EOF ; {[], [], SMap.empty}
| spec = fun_sig_item; p = parse ;
    { let spec, pred_tys = spec in
      let specs, preds, ty_map = p in
      let add_ty_map_item (pred_name, ty_list) ty_map = SMap.add pred_name ty_list ty_map in
        spec::specs, preds, List.fold_right add_ty_map_item pred_tys ty_map
    }
| pred = logical_item; p = parse ;
    { let specs, preds, ty_map = p in
        specs, pred::preds, ty_map 
    }

let fun_sig_item :=
| DECLARE ; f = ID ; LEFT_BRACE ; xs =separated_list(COMMA, arg_pair) ; RIGHT_BRACE ;
  REQUIRES ; LEFT_BRACKET;  pre = ass ; RIGHT_BRACKET;
  ENSURES ; LEFT_SQUARE ; r = arg_pair ; RIGHT_SQUARE ; LEFT_BRACKET;  post = ass ; RIGHT_BRACKET;
  { {
    fname = f;
    fvar = xs;
    fpre = pre;
    fpost = r, post;
    pnames =[];  
  }, [] }
| DECLARE ; f = ID ; LEFT_BRACE ; xs =separated_list(COMMA, arg_pair) ; RIGHT_BRACE ;
  GIVEN; pred_tys = separated_list(COMMA, pred_ty_spec);
  REQUIRES ; LEFT_BRACKET;  pre = ass ; RIGHT_BRACKET;
  ENSURES ; LEFT_SQUARE ; r = arg_pair ; RIGHT_SQUARE ; LEFT_BRACKET;  post = ass ; RIGHT_BRACKET;
  { {
    fname = f;
    fvar = xs;
    fpre = pre;
    fpost = r, post;
    pnames =[];  
  }, pred_tys }

let pred_ty_spec :=
| f = ID ; LEFT_BRACE ; xs =separated_list(COMMA, type_name) ; RIGHT_BRACE ; { f, xs }

let arg_pair :=
| x = ID ; COLON ; y = type_name ; {x, y}
| x = ID ; {x, Int}

let type_name :=
| TINT ; { (Int) }
| TBOOL ; { Bool }

let logical_item :=
| PRED ; f = ID ; LEFT_BRACE ; xs =separated_list(COMMA, arg_pair) ; RIGHT_BRACE ; HASSPEC; preds=separated_list(OR, pure_clause) ;
  { {
    pname = f;
    pargs = xs;
    pbody = preds;
  } }

let ass := 
| ps = separated_list(OR, pure_clause) ; { {
    pure =  ps ;
    spec = []
} }
| ps = separated_list(OR, pure_clause) ; WITH ; ss = separated_list(AND, spec_clause) ; { {
    pure = ps ;
    spec = ss
} }


let pure_clause :=
| TRUE; { True }
| FALSE; { False }
| p1 = pure_clause ; AND ; p2 = pure_clause ; { And (p1, p2) }
| LEFT_BRACE ; p = pure_clause ; RIGHT_BRACE; { p }
| NEG; p = pure_clause ; { Neg p }
| x = arith_clause ; { x }
| f = ID ; LEFT_BRACE ; xs =separated_list(COMMA, logical_expression) ; RIGHT_BRACE;
  { Prop (f, xs) }

let arith_clause :=
| x1 = logical_expression ; EQ ; x2 = logical_expression ;
    { Arith (Eq, x1, x2) }
| x1 = logical_expression ; LE ; x2 = logical_expression ;
    { Arith (Le, x1, x2) }


let logical_expression := 
| x = ID ; { Pvar x }
| n = INT ; { Const (Int n) }
| x1 = logical_expression ; PLUS ; x2 = logical_expression ;
    { Op (Plus, x1, x2) }
| x1 = logical_expression ; MINUS ; x2 = logical_expression ;
    { Op (Minus, x1, x2) }
| x1 = logical_expression ; MULT ; x2 = logical_expression ;
    { Op (Mult, x1, x2) }

let spec_clause :=
| f = ID ; LEFT_BRACE ; xs =separated_list(COMMA, arg_pair) ; RIGHT_BRACE ;
  HASSPEC ; LEFT_BRACKET;  pre = ass ; RIGHT_BRACKET;
  PREPOST ; r = ID  ; LEFT_BRACKET;  post = ass ; RIGHT_BRACKET;
  { {
    fname = f;
    fvar = xs;
    fpre = pre;
    fpost = (r, Int), post;
    pnames =[];  
  } }
| f = ID ; LEFT_BRACE ; xs =separated_list(COMMA, arg_pair) ; RIGHT_BRACE ;
  HASSPEC ; LEFT_BRACKET;  pre = ass ; RIGHT_BRACKET;
  PREPOST ; r = ID ; COLON ; ret_ty = type_name ; LEFT_BRACKET;  post = ass ; RIGHT_BRACKET;
  { {
    fname = f;
    fvar = xs;
    fpre = pre;
    fpost = (r, ret_ty), post;
    pnames =[];  
  } }