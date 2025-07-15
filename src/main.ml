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
  | EQUAL_EQUAL
  | EQUAL
  | BANG
  | BANG_EQUAL
  | LESS
  | LESS_EQUAL
  | GREATER
  | GREATER_EQUAL
  | EOF

type scan_result = { tokens : lexeme list; has_errors : bool }

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
  | '=' -> Some EQUAL
  | '!' -> Some BANG
  | '<' -> Some LESS
  | '>' -> Some GREATER
  | _ -> None

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
  | EQUAL_EQUAL -> "=="
  | EQUAL -> "="
  | BANG -> "!"
  | BANG_EQUAL -> "!="
  | LESS -> "<"
  | LESS_EQUAL -> "<="
  | GREATER -> ">"
  | GREATER_EQUAL -> ">="
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
  | EQUAL_EQUAL -> "EQUAL_EQUAL"
  | EQUAL -> "EQUAL"
  | BANG -> "BANG"
  | BANG_EQUAL -> "BANG_EQUAL"
  | LESS -> "LESS"
  | LESS_EQUAL -> "LESS_EQUAL"
  | GREATER -> "GREATER"
  | GREATER_EQUAL -> "GREATER_EQUAL"
  | EOF -> "EOF"

type lexer = { next_char : unit -> char; peek_char : unit -> char option }

let make_lexer str =
  let input = ref (String.to_seq str) in
  let next_char () =
    match !input () with
    | Seq.Nil -> raise End_of_file
    | Seq.Cons (c, rest) ->
        input := rest;
        c
  in
  let peek_char () =
    match !input () with Seq.Nil -> None | Seq.Cons (c, _) -> Some c
  in
  { next_char; peek_char }

let scan str =
  let lexer = make_lexer str in
  let rec scan_tokens acc has_errors =
    try
      let c = lexer.next_char () in

      let handle_x_equal_lexeme single composite =
        match lexer.peek_char () with
        | Some '=' ->
            let _ = lexer.next_char () in
            scan_tokens (composite :: acc) has_errors
        | _ -> scan_tokens (single :: acc) has_errors
      in

      match c with
      | ' ' | '\t' | '\n' | '\r' -> scan_tokens acc has_errors
      | '=' -> handle_x_equal_lexeme EQUAL EQUAL_EQUAL
      | '!' -> handle_x_equal_lexeme BANG BANG_EQUAL
      | '<' -> handle_x_equal_lexeme LESS LESS_EQUAL
      | '>' -> handle_x_equal_lexeme GREATER GREATER_EQUAL
      | '/' -> (
          match lexer.peek_char () with
          | Some '/' -> raise End_of_file
          | _ -> scan_tokens (SLASH :: acc) has_errors)
      | _ -> (
          match char_to_lexeme c with
          | Some token -> scan_tokens (token :: acc) has_errors
          | None ->
              Printf.eprintf "[line 1] Error: Unexpected character: %c\n" c;
              scan_tokens acc true)
    with End_of_file -> { tokens = List.rev (EOF :: acc); has_errors }
  in
  scan_tokens [] false

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

  let result = scan file_contents in
  List.iter
    (fun lex ->
      Printf.printf "%s %s null\n" (lexeme_display lex) (lexeme_to_str lex))
    result.tokens;

  exit (if result.has_errors then 65 else 0)
