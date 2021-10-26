%{ 

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
%token DECLARE
%start <Hiphop.Spectree.fun_signature list * Hiphop.Spectree.logical_proposition list> parse

%nonassoc EQ LE
%left AND OR
%left PLUS MINUS
%left MULT DIV
%nonassoc LEFT_BRACE
%nonassoc ID
%nonassoc TRUE FALSE INT
%%

let parse :=
| EOF ; {[], []}
| spec = fun_sig_item; p = parse ;
    { let specs, preds = p in
        spec::specs, preds 
    }


let fun_sig_item :=
| DECLARE ; f = ID ; LEFT_BRACE ; xs =separated_list(COMMA, ID) ; RIGHT_BRACE ;
  REQUIRES ; LEFT_BRACKET;  pre = ass ; RIGHT_BRACKET;
  ENSURES ; LEFT_SQUARE ; r = ID ; RIGHT_SQUARE ; LEFT_BRACKET;  post = ass ; RIGHT_BRACKET;
  { {
    fname = f;
    fvar = List.rev xs;
    fpre = pre;
    fpost = r, post;
    pnames =[];  
  } }

let ass := 
| TRUE ; { {
    pure =  [ True ];
    spec = []
} }