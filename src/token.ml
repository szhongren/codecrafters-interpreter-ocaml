module Token = struct
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
    | STRING of string
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
    | STRING value -> "\"" ^ value ^ "\""
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
    | STRING _ -> "STRING"
    | EOF -> "EOF"

  let lexeme_value = function STRING str -> str | _ -> "null"
end
