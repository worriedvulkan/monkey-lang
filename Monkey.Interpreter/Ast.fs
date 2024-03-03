module Monkey.Interpreter.Ast

open System.Collections.Generic
open System.Text

[<RequireQualifiedAccess>]
type Expression =
    | Identifier of Identifier
    | Integer of Integer
    | Empty

and Identifier = { Token : Token ; Value : string }
and Integer = { Token : Token ; Value : int }


[<RequireQualifiedAccess>]
type Statement =
    | Let of LetStmt
    | Return of ReturnStmt
    | Expression of ExpressionStmt

and LetStmt = { Token : Token ; Name : Identifier ; Value : Expression }
and ReturnStmt = { Token : Token ; ReturnValue : Expression }
and ExpressionStmt = {Token : Token ; Expr : Expression}

type Program = { Statements : List<Statement> }

let expressionTokenLiteral (expr : Expression) =
    match expr with
    | Expression.Identifier ident -> ident.Token.Literal
    | Expression.Integer i -> i.Token.Literal
    | Expression.Empty -> ""

let statementTokenLiteral (stmt : Statement) =
    match stmt with
    | Statement.Let letStmt -> letStmt.Token.Literal
    | Statement.Expression exprStmt -> exprStmt.Token.Literal
    | Statement.Return returnStmt -> returnStmt.Token.Literal

let expressionToString (expr : Expression) =
    match expr with
    | Expression.Empty -> ""
    | Expression.Identifier ident -> ident.Value
    | Expression.Integer num -> num.Value.ToString()

let statementToString (stmt : Statement) =
    match stmt with
    | Statement.Let letStmt ->
        letStmt.Token.Literal
        + " "
        + (expressionToString (Expression.Identifier letStmt.Name))
        + " = "
        + (expressionToString letStmt.Value)
        + ";"
    | Statement.Return returnStmt -> returnStmt.Token.Literal + " " + (expressionToString returnStmt.ReturnValue) + ";"
    | Statement.Expression exprStmt -> expressionToString exprStmt.Expr
    
let programToString (program: Program) =
    let builder = StringBuilder()
    for s in program.Statements do
        builder.Append(statementToString s) |> ignore
        
    builder.ToString()
