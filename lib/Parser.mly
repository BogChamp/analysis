%{
open Syntax
%}

%token EOF

%token <string> Identifier

%token NilLiteral
%token <bool> BooleanLiteral
%token <int> IntegerLiteral
%token If
%token Then
%token Else
%token While
%token Do
%token End
%token Input
%token Print
%token LeftParenthesis
%token RightParenthesis
%token Add
%token Subtract
%token Multiply
%token Divide
%token Equal
%token NotEqual
%token Less
%token LessOrEqual
%token Greater
%token GreaterOrEqual
%token AssignmentOperator

%left Equal NotEqual Less LessOrEqual Greater GreaterOrEqual
%left Add Subtract
%left Multiply Divide

%start <Program.t> program

%%

let program :=
  | ~ = blockstatement; EOF;
    <>

let blockstatement :=
  | ~ = statement*;
    <>

let if_statement :=
  | If; statement = expression; Then; actions = blockstatement; Else?; elseactions = blockstatement?;End;
    {
      Statement.If {statement; actions; elseactions}
    }

let assignmentstatement :=
  | identifier = Identifier; AssignmentOperator; value = expression;
    {
      Statement.AssignmentStatement {identifier; value} 
    }

let whilestatement :=
  | While; statement = expression; Do; actions = blockstatement; End;
    {
      Statement.WhileStatement {statement; actions}
    }

let printstatement :=
  | Print; LeftParenthesis; expr = expression; RightParenthesis;
    {
      Statement.PrintStatement expr
    }

let statement :=
  | ~ = assignmentstatement;
    <>
  | ~ = if_statement;
    <>
  | ~ = whilestatement;
    <>
  | ~ = printstatement;
    <>

let parenthesizedexpression :=
  | LeftParenthesis; expr = expression; RightParenthesis;
    {
      Expression.ParenthesizedExpression expr
    }

let inputexpression :=
  | Input; LeftParenthesis; RightParenthesis;
    {
      Expression.InputExpression
    }

let expression :=
  | ~ = Identifier;
    <Expression.Identifier>
  | ~ = literal;
    <>
  | ~ = add_sub_operation;
    <>
  | ~ = mul_div_operation;
    <>
  | ~ = comp_operation;
    <>
  | ~ = parenthesizedexpression;
    <>
  | ~ = inputexpression;
    <>

let add_sub_expression :=
  | ~ = Identifier;
    <Expression.Identifier>
  | ~ = literal;
    <>
  | ~ = add_sub_operation;
    <>
  | ~ = mul_div_operation;
    <>
  | ~ = parenthesizedexpression;
    <>
  | ~ = inputexpression;
    <>

let mul_div_expression :=
  | ~ = Identifier;
    <Expression.Identifier>
  | ~ = literal;
    <>
  | ~ = mul_div_operation;
    <>
  | ~ = parenthesizedexpression;
    <>
  | ~ = inputexpression;
    <>

let comp_operation :=
  | left = expression; operator = comp_operator; right = expression;
    {
      Expression.BinaryExpression {left; operator; right}
    }

let add_sub_operation :=
  | left = add_sub_expression; operator = add_sub_operator; right = add_sub_expression;
    {
      Expression.BinaryExpression {left; operator; right}
    }

let mul_div_operation :=
  | left = mul_div_expression; operator = mul_div_operator; right = mul_div_expression;
    {
      Expression.BinaryExpression {left; operator; right}
    }

let literal ==
  | NilLiteral;
    {
      Expression.Literal Expression.Literal.Nil
    }
  | value = BooleanLiteral;
    {
      Expression.Literal (Expression.Literal.Boolean value)
    }
  | value = IntegerLiteral;
    {
      Expression.Literal (Expression.Literal.Integer value)
    }

let add_sub_operator ==
  | Add;
    {
      Expression.BinaryOperator.Add 
    }
  | Subtract;
    {
      Expression.BinaryOperator.Subtract
    }

let mul_div_operator ==
  | Multiply;
    {
      Expression.BinaryOperator.Multiply
    }
  | Divide;
    {
      Expression.BinaryOperator.Divide
    }

let comp_operator ==
  | Equal;
    {
      Expression.BinaryOperator.Equal
    }
  | NotEqual;
    {
      Expression.BinaryOperator.NotEqual
    }
  | Less;
    {
      Expression.BinaryOperator.Less
    }
  | LessOrEqual;
    {
      Expression.BinaryOperator.LessOrEqual
    }
  | Greater;
    {
      Expression.BinaryOperator.Greater
    }
  | GreaterOrEqual;
    {
      Expression.BinaryOperator.GreaterOrEqual
    }
