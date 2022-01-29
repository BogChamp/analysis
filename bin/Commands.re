open MuLua;

let parse_file = path =>
  path
  |> Parser.parse_file
  |> Base.Result.map_error(
       ~f=
         fun
         | `SystemError(message) => Stdio.prerr_endline(message)
         | `SyntaxError({start, current}) =>
           Stdio.eprintf(
             "%s:%d:%d-%d:%d: Syntax error\n",
             path,
             start.pos_lnum,
             start.pos_cnum,
             current.pos_lnum,
             current.pos_cnum,
           ),
     );

let run_file = parse_file %> Base.Result.map(~f=Runtime.eval);

let print_ast = {
  let to_string = Syntax.Program.sexp_of_t %> Base.Sexp.to_string_hum;
  parse_file %> Base.Result.map(~f=to_string %> Stdio.print_endline);
};

let make_analysis = {
  parse_file %> Base.Result.map(~f=CFG.make_analysis);
};
