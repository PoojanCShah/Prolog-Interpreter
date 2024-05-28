%{


type exp = 
  | Var of string
  | Const of string
  | Num of int
  | Term of string * (exp list)
  | PList of (exp list)
  | CUT 
  | FAIL

type clause =  Clause of exp * (exp list)
type goal =  Goal of exp list


%}

%token <string> VAR CONST 
%token <int> NUM
%token LPAREN RPAREN CUTTOKEN FAILTOKEN COMMA COLONDASH DOT EOF LBRACKET RBRACKET PIPE PLUS IS MINUS TIMES GT LT EQ NEQ DEQ NDEQ

%start program goal
%type <exp list> goal
%type <clause list> program
%nonassoc LPAREN RPAREN
%nonassoc IS
%nonassoc GT LT EQ NEQ DEQ NDE
%right COMMA 
%left PLUS MINUS
%left TIMES
%nonassoc COLONDASH
%nonassoc DOT

%%

program :
    | clause DOT {[$1]}
    | clause DOT program { $1 :: $3 }
    ;
 
clause :
    | head { Clause($1, [])}
    | head COLONDASH body { Clause($1, $3) }
    ;

head :
    | exp { $1 }
    ;

body :
    | exp { [$1] }
    | exp COMMA body { $1 :: $3 }
    ;

exp :
    | VAR { Var($1) }
    | CONST { Const($1) }
    | NUM { Num($1) }
    | CONST LPAREN terms RPAREN { Term($1, $3) }
    | CUTTOKEN { CUT }
    | FAILTOKEN { FAIL }
    | LBRACKET RBRACKET {Term("List",[])}
    | LBRACKET exp PIPE exp RBRACKET { Term("List", [$2; $4 ]) }
    | exp PLUS exp { Term("Plus", [$1; $3]) }
    | exp MINUS exp { Term("Minus", [$1; $3]) }
    | exp TIMES exp { Term("Times", [$1; $3]) }
    | exp IS exp { Term("Is", [$1; $3]) }
    | exp GT exp { Term("Greater", [$1; $3]) }
    | exp LT exp { Term("Lesser", [$1; $3]) }
    | exp EQ exp { Term("Equal", [$1; $3]) }
    | exp NEQ exp { Term("NotEqual", [$1; $3]) }
    | exp DEQ exp { Term("DeepEqual", [$1; $3]) }
    | exp NDEQ exp { Term("NotDeepEqual", [$1; $3]) }
    | LPAREN exp RPAREN { $2 }




terms :
    | exp { [$1] }
    | exp COMMA terms { $1 :: $3 }
    ;

goal :
    | exp DOT { [$1] }
    | exp COMMA goal { $1 :: $3}
    ;


