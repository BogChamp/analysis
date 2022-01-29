type lexing_segment = {
  start: Lexing.position,
  current: Lexing.position,
};

let parse_lexbuf = lexbuf => {
  open Parser_Menhir;
  let parse =
    Sedlexing.with_tokenizer(Lexer.token)
    %> MenhirLib.Convert.Simplified.traditional2revised(program);
  try(Ok(lexbuf |> parse)) {
  | Lexer.Unexpected(_)
  | Error =>
    let (start, current) = Sedlexing.lexing_positions(lexbuf);
    Error(`SyntaxError({start, current}));
  };
};

let parse_channel = Sedlexing.Utf8.from_channel %> parse_lexbuf;

let parse_file = filename =>
  try(Core.In_channel.with_file(filename, ~f=parse_channel)) {
  | Sys_error(message) => Error(`SystemError(message))
  };
