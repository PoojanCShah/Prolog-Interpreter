{
    open Parser
    exception LexingError of string
}

let white = [' ' '\t' '\n' '\r']+
let const = ['a'-'z']+ ['a'-'z']* ['A'-'Z']* ['0'-'9']*
let var = ['A'-'Z']+ ['A'-'Z']* ['a'-'z']* ['0'-'9']* | '_'
let num = ['0'-'9']+


rule comment = parse
| "*/" { read lexbuf }
| _ { comment lexbuf }
| eof { raise (LexingError "Unclosed comment") }

and

read = 
parse
| "is" {IS}
| white {read lexbuf}
| "fail" {FAILTOKEN}
| "!" {CUTTOKEN}
| const {CONST (Lexing.lexeme lexbuf)}
| var {VAR (Lexing.lexeme lexbuf)}
| num {NUM (int_of_string (Lexing.lexeme lexbuf))}
| "/=" {NEQ}
| "==" {DEQ}
| "=/=" {NDEQ}
| '.' {DOT}
| ',' {COMMA}
| '(' {LPAREN}
| ')' {RPAREN}
| '[' {LBRACKET}
| ']' {RBRACKET}
| '|' {PIPE}
| ":-" {COLONDASH}
| "+" {PLUS}
| "-" {MINUS}
| "*" {TIMES}
| ">" {GT}
| "<" {LT}
| "=" {EQ}
| "/*" { comment lexbuf }
| eof {EOF}
| _  { raise (LexingError ("Lexing Error : invalid token  " ^ Lexing.lexeme lexbuf))}

{
    let token_to_string = function
    | CONST s -> "CONST " ^ s
    | VAR s -> "VAR " ^ s
    | DOT -> "DOT"
    | COMMA -> "COMMA"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | COLONDASH -> "COLONDASH"
    | FAILTOKEN -> "FAILTOKEN"
    | CUTTOKEN -> "CUTTOKEN"
    | EOF -> "EOF"
    | LBRACKET -> "LBRACKET"
    | RBRACKET -> "RBRACKET"
    | PIPE -> "PIPE"
    | PLUS -> "PLUS"
    | NUM n -> "NUM " ^ string_of_int n
    | IS -> "IS"
    | MINUS -> "MINUS"
    | TIMES -> "TIMES"
    | GT -> "GT"
    | LT -> "LT"
    | EQ -> "EQ"
    | NEQ -> "NEQ"
    | DEQ -> "DEQ"
    | NDEQ -> "NDEQ"

}