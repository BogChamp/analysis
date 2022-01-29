open Cmdliner;

let () = {
  let mulua = {
    let doc = "muLua programming language.";
    Term.(ret(const(`Help((`Auto, None)))), info("mulua", ~doc));
  };

  let commands = {
    let to_exit_status = Result.is_error %> Bool.to_int;

    let program = {
      let doc = "Program path.";
      let docv = "PROGRAM";
      Arg.(required & pos(0, some(string), None) & info([], ~doc, ~docv));
    };

    let run = {
      let doc = "Run the program.";
      Term.(
        const(Commands.run_file %> to_exit_status) $ program,
        info("run", ~doc),
      );
    };

    let ast = {
      let doc = "Print AST of the program.";
      Term.(
        const(Commands.print_ast %> to_exit_status) $ program,
        info("ast", ~doc),
      );
    };

    let cfg = {
      let doc = "Make cfg";
      Term.(
        const(Commands.make_analysis %> to_exit_status) $ program,
        info("cfg", ~doc),
      );
    };

    [run, ast, cfg];
  };

  Term.(eval_choice(mulua, commands) |> exit_status);
};
