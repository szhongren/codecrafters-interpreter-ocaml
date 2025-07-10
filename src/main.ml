type lexeme = LEFT_PAREN | RIGHT_PAREN | EOF

let char_to_lexeme = function
  | '(' -> LEFT_PAREN
  | ')' -> RIGHT_PAREN
  | _ -> EOF

let lexeme_to_str = function
  | LEFT_PAREN -> "("
  | RIGHT_PAREN -> ")"
  | EOF -> ""

let lexeme_display = function
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | EOF -> "EOF"

let rec scan str =
  if String.length str == 0 then []
  else char_to_lexeme str.[0] :: scan (String.sub str 1 (String.length str - 1))

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);

  let command = Sys.argv.(1) in
  let filename = Sys.argv.(2) in

  if command <> "tokenize" then (
    Printf.eprintf "Unknown command: %s\n" command;
    exit 1);

  let file_contents = In_channel.with_open_text filename In_channel.input_all in

  (* You can use print statements as follows for debugging, they'll be visible when running tests. *)
  Printf.eprintf "Logs from your program will appear here!\n";

  List.iter
    (fun lex ->
      Printf.printf "%s %s null\n" (lexeme_display lex) (lexeme_to_str lex))
    (scan file_contents)
