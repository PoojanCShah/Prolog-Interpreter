open Parser
open Lexer

let read_program filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  try
    let program = Parser.program Lexer.read lexbuf in
    close_in ic;
    program
  with
  | Lexer.LexingError msg ->
    close_in ic;
    failwith msg
  | _ -> failwith "Syntax error"




  let rec read_query () =
    print_string "?- ";
    flush stdout;
    let line = read_line () in
    let lexbuf = Lexing.from_string line in
    if line = "quit." then exit 0
    else
      (try
         let query = Parser.goal Lexer.read lexbuf in
         query
       with
       | Parsing.Parse_error ->
         print_endline "Syntax error";
         read_query ()
       | _ ->
         print_endline "An error occurred";
         exit 1
      )
  
  

let rec repl db =
  let query = read_query () in
    Intrprt.interpret db query;
    repl db

let _ =
  let filename = Sys.argv.(1) in
  let program = read_program filename in
  repl program