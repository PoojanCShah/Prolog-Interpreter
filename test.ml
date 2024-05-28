open Parser
open Lexer
(*

take input from use and print the parse tree

*)

let rec print_exp e = 
  match e with
  | Const s -> print_string ("Const(" ^ s ^")")
  | Var s -> print_string ("Var(" ^ s ^")")
  | PList l -> print_string "List(["; List.iter print_exp l; print_string "])"
  | Term (s, l) -> print_string ("Term(" ^ s ^ ", ("); List.iter print_exp l; print_string ")"
  | CUT -> print_string "CUT"
  | FAIL -> print_string "FAIL"
  | Num n -> print_string ("Num(" ^ string_of_int n ^ ")")


let print_clause c = 
  match c with Clause (h, b) -> print_exp h; print_string ":-";
  List.iter print_exp b;
  print_newline()

let print_program p = 
  List.iter print_clause p

let print_tree lexbuf = 
  let e = Parser.program Lexer.read lexbuf in
  print_program e;
  print_newline()


  (* take a string and print its tokens*)
let rec print_tokens s = 
  let lexbuf = Lexing.from_string s in
  let rec print_tokens' lexbuf = 
    let tok = Lexer.read lexbuf in
    match tok with
    | EOF -> print_newline()
    | _ -> print_string (Lexer.token_to_string tok); print_string " "; print_tokens' lexbuf
  in
  print_tokens' lexbuf




let () =
let s = "a(3)." in
  print_tokens s ;
  print_tree (Lexing.from_string s)