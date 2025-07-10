type lexeme =
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | EOF

let char_to_lexeme = function
  | ',' -> Some COMMA
  | '.' -> Some DOT
  | '-' -> Some MINUS
  | '+' -> Some PLUS
  | ';' -> Some SEMICOLON
  | '/' -> Some SLASH
  | '*' -> Some STAR
  | '(' -> Some LEFT_PAREN
  | ')' -> Some RIGHT_PAREN
  | '{' -> Some LEFT_BRACE
  | '}' -> Some RIGHT_BRACE
  | c ->
      Printf.eprintf "[line 1] Error: Unexpected character: %c\n" c;
      None

let lexeme_to_str = function
  | COMMA -> ","
  | DOT -> "."
  | MINUS -> "-"
  | PLUS -> "+"
  | SEMICOLON -> ";"
  | SLASH -> "/"
  | STAR -> "*"
  | LEFT_PAREN -> "("
  | RIGHT_PAREN -> ")"
  | LEFT_BRACE -> "{"
  | RIGHT_BRACE -> "}"
  | EOF -> ""

let lexeme_display = function
  | COMMA -> "COMMA"
  | DOT -> "DOT"
  | MINUS -> "MINUS"
  | PLUS -> "PLUS"
  | SEMICOLON -> "SEMICOLON"
  | SLASH -> "SLASH"
  | STAR -> "STAR"
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | EOF -> "EOF"

let scan str =
  let input = ref (String.to_seq str) in
  (* this is a generator that captures the input above *)
  let next_char () =
    match !input () with
    | Seq.Nil -> raise End_of_file
    | Seq.Cons (c, rest) ->
        input := rest;
        c
  in
  let rec scan_tokens acc =
    try
      let c = next_char () in
      match c with
      | ' ' | '\t' | '\n' | '\r' -> scan_tokens acc
      | _ -> (
          match char_to_lexeme c with
          | Some token -> scan_tokens (token :: acc)
          | None -> scan_tokens acc)
    with End_of_file -> List.rev (EOF :: acc)
  in
  scan_tokens []

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
