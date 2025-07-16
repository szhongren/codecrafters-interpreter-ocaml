open Token

type scan_result = { tokens : Token.lexeme list; has_errors : bool }

type lexer = {
  line_number : int ref;
  next_char : unit -> char;
  peek_char : unit -> char option;
}

let make_lexer str =
  let input = ref (String.to_seq str) in
  let line_number = ref 1 in
  let next_char () =
    match !input () with
    | Seq.Nil -> raise End_of_file
    | Seq.Cons (c, rest) ->
        if c == '\n' then line_number := !line_number + 1;
        input := rest;
        c
  in
  let peek_char () =
    match !input () with Seq.Nil -> None | Seq.Cons (c, _) -> Some c
  in
  { line_number; next_char; peek_char }

let scan str =
  let lexer = make_lexer str in
  let rec scan_tokens has_errors acc =
    try
      let c = lexer.next_char () in

      let handle_x_equal_lexeme single composite =
        match lexer.peek_char () with
        | Some '=' ->
            let _ = lexer.next_char () in
            scan_tokens has_errors (composite :: acc)
        | _ -> scan_tokens has_errors (single :: acc)
      in

      let scan_comment () =
        while lexer.next_char () <> '\n' do
          ()
        done;
        acc
      in

      let scan_string () =
        try
          let str = ref "" in
          while lexer.peek_char () <> Some '"' do
            str := !str ^ String.make 1 (lexer.next_char ())
          done;
          let _ = lexer.next_char () in
          (Token.STRING !str :: acc, false)
        with End_of_file ->
          Printf.eprintf "[line %d] Error: Unterminated string.\n"
            !(lexer.line_number);
          (acc, true)
      in

      match c with
      | ' ' | '\t' | '\n' | '\r' -> scan_tokens has_errors acc
      | '=' -> handle_x_equal_lexeme EQUAL EQUAL_EQUAL
      | '!' -> handle_x_equal_lexeme BANG BANG_EQUAL
      | '<' -> handle_x_equal_lexeme LESS LESS_EQUAL
      | '>' -> handle_x_equal_lexeme GREATER GREATER_EQUAL
      | '"' ->
          let string_result, string_error = scan_string () in
          scan_tokens (has_errors || string_error) string_result
      | '/' -> (
          match lexer.peek_char () with
          | Some '/' -> scan_comment () |> scan_tokens has_errors
          | _ -> scan_tokens has_errors (SLASH :: acc))
      | _ -> (
          match Token.char_to_lexeme c with
          | Some token -> scan_tokens has_errors (token :: acc)
          | None ->
              Printf.eprintf "[line %d] Error: Unexpected character: %c\n"
                !(lexer.line_number) c;
              scan_tokens true acc)
    with End_of_file -> { tokens = List.rev (Token.EOF :: acc); has_errors }
  in
  scan_tokens false []

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
      Printf.printf "%s %s %s\n" (Token.lexeme_display lex)
        (Token.lexeme_to_str lex) (Token.lexeme_value lex))
    result.tokens;

  exit (if result.has_errors then 65 else 0)
