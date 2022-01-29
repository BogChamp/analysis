open Base;

module Identifier = String;

module Expression = {
  module Literal = {
    [@deriving sexp_of]
    type t =
      | Nil
      | Boolean(bool)
      | Integer(int);
  };

  module BinaryOperator = {
    [@deriving sexp_of]
    type t =
      | Add
      | Subtract
      | Multiply
      | Divide
      | Equal
      | NotEqual
      | Less
      | LessOrEqual
      | Greater
      | GreaterOrEqual;
  };

  [@deriving sexp_of]
  type t =
    | Identifier(Identifier.t)
    | Literal(Literal.t)
    | BinaryExpression({
        left: t,
        operator: BinaryOperator.t,
        right: t,
      })
    | ParenthesizedExpression(t)
    | InputExpression;
};

module rec BlockStatement: {
  [@deriving sexp_of]
  type t = list(Statement.t);
} = {
  [@deriving sexp_of]
  type t = list(Statement.t);
}
and Statement: {

  [@deriving sexp_of]
  type t =
    | AssignmentStatement({ 
        identifier: Identifier.t,
        value: Expression.t,
    })
    | If({
        statement: Expression.t,
        actions: BlockStatement.t,
        elseactions: option(BlockStatement.t),
    })
    | WhileStatement({
        statement: Expression.t,
        actions: BlockStatement.t,
    })
    | PrintStatement(Expression.t);
} = {

  [@deriving sexp_of]
  type t =
    | AssignmentStatement({ 
        identifier: Identifier.t,
        value: Expression.t,
    })
    | If({
        statement: Expression.t,
        actions: BlockStatement.t,
        elseactions: option(BlockStatement.t),
    })
    | WhileStatement({
      statement: Expression.t,
      actions: BlockStatement.t,
    })
    | PrintStatement(Expression.t);
};

module Program = {
  [@deriving sexp_of]
  type t = BlockStatement.t;
};
