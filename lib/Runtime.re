open Base;

module Value = {
  [@deriving sexp_of]
  type t =
    | Nil
    | Boolean(bool)
    | Integer(int);

  let to_string =
    fun
    | Nil => "nil"
    | Boolean(x) => Bool.to_string(x)
    | Integer(x) => Int.to_string(x);
};


module Environment: {
  type t = Map.M(Syntax.Identifier).t(Value.t);
  let empty: t;
} = {
  type t = Map.M(Syntax.Identifier).t(Value.t);
  let empty: t = Map.empty((module Syntax.Identifier));
};

let rec calculate_expression = (expression: Syntax.Expression.t, environment: Environment.t): Value.t => {
  switch (expression) {
    | Syntax.Expression.Literal(literal) => {
      switch (literal) {
        | Syntax.Expression.Literal.Nil => Value.Nil
        | Syntax.Expression.Literal.Boolean(literal) => Value.Boolean(literal)
        | Syntax.Expression.Literal.Integer(literal) => Value.Integer(literal)
      };
    }
    | Syntax.Expression.BinaryExpression({left, operator, right}) => {
      switch (operator) {
        | Syntax.Expression.BinaryOperator.Add => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          switch (left_expression, right_expression) {
            | (Value.Integer(left_expression), Value.Integer(right_expression)) => Value.Integer(left_expression + right_expression)
            | _ => failwith("")
          };
        }
        | Syntax.Expression.BinaryOperator.Subtract => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          switch (left_expression, right_expression) {
            | (Value.Integer(left_expression), Value.Integer(right_expression)) => Value.Integer(left_expression - right_expression)
            | _ => failwith("")
          };
        }
        | Syntax.Expression.BinaryOperator.Multiply => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          switch (left_expression, right_expression) {
            | (Value.Integer(left_expression), Value.Integer(right_expression)) => Value.Integer(left_expression * right_expression)
            | _ => failwith("")
          };
        }
        | Syntax.Expression.BinaryOperator.Divide => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          switch (left_expression, right_expression) {
            | (Value.Integer(left_expression), Value.Integer(right_expression)) => {
              switch (right_expression) {
                | 0 => failwith("")
                | _ => Value.Integer(left_expression / right_expression)
              };
            }
            | _ => failwith("")
          };
        }

        | Syntax.Expression.BinaryOperator.Equal => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          Value.Boolean(Poly.(left_expression == right_expression));
        }
        | Syntax.Expression.BinaryOperator.NotEqual => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          Value.Boolean(Poly.(left_expression != right_expression));
        }

        | Syntax.Expression.BinaryOperator.Less => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          switch (left_expression, right_expression) {
            | (Value.Integer(left_expression), Value.Integer(right_expression)) => Value.Boolean(left_expression < right_expression)
            | _ => failwith("")
          };
        }
        | Syntax.Expression.BinaryOperator.LessOrEqual => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          switch (left_expression, right_expression) {
            | (Value.Integer(left_expression), Value.Integer(right_expression)) => Value.Boolean(left_expression <= right_expression)
            | _ => failwith("")
          };
        }
        | Syntax.Expression.BinaryOperator.Greater => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          switch (left_expression, right_expression) {
            | (Value.Integer(left_expression), Value.Integer(right_expression)) => Value.Boolean(left_expression > right_expression)
            | _ => failwith("")
          };
        }
        | Syntax.Expression.BinaryOperator.GreaterOrEqual => {
          let left_expression = calculate_expression(left, environment);
          let right_expression = calculate_expression(right, environment);
          switch (left_expression, right_expression) {
            | (Value.Integer(left_expression), Value.Integer(right_expression)) => Value.Boolean(left_expression >= right_expression)
            | _ => failwith("")
          };
        }
      };
    }
    | Syntax.Expression.Identifier(id) => {
      let value = Map.find(environment, id);
      switch (value) {
        | None => Value.Nil
        | Some(value) => value
      };
    }
    | Syntax.Expression.ParenthesizedExpression(expression) => calculate_expression(expression, environment)
    | Syntax.Expression.InputExpression => Value.Integer(Caml.read_int())
  };
};

let rec eval = (~environment=Environment.empty, _program: Syntax.Program.t): Environment.t => {
  List.fold(_program, ~init=environment, ~f=(init: Environment.t, statement: Syntax.Statement.t) => {
    switch (statement) {
      | Syntax.Statement.AssignmentStatement({identifier, value}) =>  {
        Map.set(~key=identifier, ~data=calculate_expression(value, init), init);
      }
      | Syntax.Statement.PrintStatement(expression) => {
        let printed = calculate_expression(expression, init);
        Stdio.print_string(Value.to_string(printed) ++ "\n");
        init;
      }
      | Syntax.Statement.If({statement, actions, elseactions}) => {
        let expression = calculate_expression(statement, init);
        if (Poly.(expression != Value.Nil) && Poly.(expression != Value.Boolean(false))) {
          eval(~environment = init, actions);
        } else {
          switch (elseactions) {
            | None => init;
            | Some(elseactions) => eval(~environment = init, elseactions)
          };
        }
      }
      | Syntax.Statement.WhileStatement({statement, actions}) => {
        
        eval_body(init, statement, actions);
      }
    };
  });
}
and eval_body = (env: Environment.t, expr: Syntax.Expression.t, block: Syntax.BlockStatement.t): Environment.t => {
  let expression = calculate_expression(expr, env);
  if (Poly.(expression == Value.Nil) || Poly.(expression == Value.Boolean(false))) {
    env;
  } else {
  let new_eval = eval(~environment=env, block);
  eval_body(new_eval, expr, block);   
  };
};

