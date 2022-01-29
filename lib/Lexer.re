let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z'];
let digit = [%sedlex.regexp? '0' .. '9'];

let identifier_start = [%sedlex.regexp? letter | '_'];
let identifier_continue = [%sedlex.regexp? identifier_start | digit];
let identifier = [%sedlex.regexp?
  (identifier_start, Star(identifier_continue))
];

let integer = [%sedlex.regexp? (Rep('-', 0 .. 1), Plus(digit))]

exception Unexpected(string);

let rec token = lexbuf => {
  open Parser_Menhir;
  open Sedlexing.Utf8;
  switch%sedlex (lexbuf) {
  | white_space => token(lexbuf)
  | "nil" => NilLiteral
  | "true" => BooleanLiteral(true)
  | "false" => BooleanLiteral(false)
  | "if" => If
  | "then" => Then
  | "else" => Else
  | "while" => While
  | "do" => Do
  | "end" => End
  | "input" => Input
  | "print" => Print
  | "(" => LeftParenthesis
  | ")" => RightParenthesis
  | "+" => Add
  | "-" => Subtract
  | "*" => Multiply
  | "//" => Divide
  | "==" => Equal
  | "~=" => NotEqual
  | "<" => Less
  | "<=" => LessOrEqual
  | ">" => Greater
  | ">=" => GreaterOrEqual
  | "=" => AssignmentOperator
  | integer => IntegerLiteral(int_of_string(lexeme(lexbuf)))
  | identifier => Identifier(lexeme(lexbuf))
  | eof => EOF
  | _ => raise(Unexpected(lexeme(lexbuf)))
  };
};
