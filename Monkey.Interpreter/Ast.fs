module Monkey.Interpreter.Ast

open System.Collections.Generic
open System.Text

[<RequireQualifiedAccess>]
type Expression =
    | Ident of Identifier
    | Int of Integer
    | Prefix of PrefixExpr
    | Infix of InfixExpr
    | Empty

and Identifier = { Token : Token ; Value : string }
and Integer = { Token : Token ; Value : int }
and PrefixExpr = { Token : Token ; Operator : string ; Right : Expression }
and InfixExpr = { Token : Token ; Left : Expression ; Operator : string ; Right : Expression }

[<RequireQualifiedAccess>]
type Statement =
    | Let of LetStmt
    | Return of ReturnStmt
    | Expr of ExpressionStmt

and LetStmt = { Token : Token ; Name : Identifier ; Value : Expression }
and ReturnStmt = { Token : Token ; ReturnValue : Expression }
and ExpressionStmt = { Token : Token ; ChildExpr : Expression option }

type Program = { Statements : List<Statement> }

let expressionTokenLiteral (expr : Expression) =
    match expr with
    | Expression.Empty -> ""
    | Expression.Ident ident -> ident.Token.Literal
    | Expression.Int i -> i.Token.Literal
    | Expression.Prefix pre -> pre.Token.Literal
    | Expression.Infix infix -> infix.Token.Literal

let statementTokenLiteral (stmt : Statement) =
    match stmt with
    | Statement.Let letStmt -> letStmt.Token.Literal
    | Statement.Expr exprStmt -> exprStmt.Token.Literal
    | Statement.Return returnStmt -> returnStmt.Token.Literal

let rec expressionToString (expr : Expression) =
    match expr with
    | Expression.Empty -> ""
    | Expression.Ident ident -> ident.Value
    | Expression.Int num -> num.Value.ToString ()
    | Expression.Prefix pre -> "(" + pre.Operator + expressionToString pre.Right + ")"
    | Expression.Infix infix ->
        "("
        + expressionToString infix.Left
        + " "
        + infix.Operator
        + " "
        + expressionToString infix.Right
        + ")"

let statementToString (stmt : Statement) =
    match stmt with
    | Statement.Let letStmt ->
        letStmt.Token.Literal
        + " "
        + (expressionToString (Expression.Ident letStmt.Name))
        + " = "
        + (expressionToString letStmt.Value)
        + ";"
    | Statement.Return returnStmt ->
        returnStmt.Token.Literal
        + " "
        + (expressionToString returnStmt.ReturnValue)
        + ";"
    | Statement.Expr exprStmt ->
        exprStmt.ChildExpr |> Option.map expressionToString |> Option.defaultValue ""

let programToString (program : Program) =
    let builder = StringBuilder ()

    for s in program.Statements do
        builder.Append (statementToString s) |> ignore

    builder.ToString ()
