module Monkey.Interpreter.Ast

open System.Collections.Generic
open System.Text
open System
open System.Linq

type Expression =
    | Ident of Identifier
    | Int of Integer
    | Bool of Boolean
    | Prefix of PrefixExpr
    | Infix of InfixExpr
    | If of IfExpr
    | Func of FuncExpr
    | Call of CallExpr
    | Empty

and Identifier = { Token : Token ; Value : string }
and Integer = { Token : Token ; Value : int }
and Boolean = { Token : Token ; Value : bool }
and PrefixExpr = { Token : Token ; Operator : string ; Right : Expression }
and InfixExpr = { Token : Token ; Left : Expression ; Operator : string ; Right : Expression }
and FuncExpr = { Token : Token ; Parameters : List<Identifier> ; Body : BlockStmt }
and CallExpr = { Token : Token ; Function : Expression ; Arguments : List<Expression> }

and IfExpr = {
    Token : Token
    Condition : Expression
    Consequence : BlockStmt
    Alternative : BlockStmt option
}

and Statement =
    | Let of LetStmt
    | Return of ReturnStmt
    | Expr of ExpressionStmt
    | Block of BlockStmt

and LetStmt = { Token : Token ; Name : Identifier ; Value : Expression }
and ReturnStmt = { Token : Token ; ReturnValue : Expression }
and ExpressionStmt = { Token : Token ; ChildExpr : Expression option }
and BlockStmt = { Token : Token ; Statements : List<Statement> }

type Program = { Statements : List<Statement> }

let expressionTokenLiteral (expr : Expression) =
    match expr with
    | Empty -> ""
    | Ident ident -> ident.Token.Literal
    | Int i -> i.Token.Literal
    | Bool b -> b.Token.Literal
    | Prefix pre -> pre.Token.Literal
    | Infix infix -> infix.Token.Literal
    | If ifExpr -> ifExpr.Token.Literal
    | Func func -> func.Token.Literal
    | Call call -> call.Token.Literal

let statementTokenLiteral (stmt : Statement) =
    match stmt with
    | Let letStmt -> letStmt.Token.Literal
    | Expr exprStmt -> exprStmt.Token.Literal
    | Return returnStmt -> returnStmt.Token.Literal
    | Block blockStmt -> blockStmt.Token.Literal

let rec expressionToString (expr : Expression) =
    let sb = StringBuilder ()

    match expr with
    | Empty -> ()
    | Ident ident -> sb.Append ident.Value |> ignore
    | Int num -> sb.Append num.Value |> ignore
    | Bool boolean -> sb.Append boolean.Value |> ignore
    | Prefix pre ->
        sb.Append "(" |> ignore
        sb.Append pre.Operator |> ignore
        let str : string = expressionToString pre.Right
        sb.Append str |> ignore
        sb.Append ")" |> ignore
    | Infix infix ->
        sb.Append "(" |> ignore
        sb.Append (expressionToString infix.Left) |> ignore
        sb.Append " " |> ignore
        sb.Append infix.Operator |> ignore
        sb.Append " " |> ignore
        sb.Append (expressionToString infix.Right) |> ignore
        sb.Append ")" |> ignore
    | If ifExpr ->
        sb.Append "if" |> ignore
        sb.Append (expressionToString ifExpr.Condition) |> ignore
        sb.Append " " |> ignore
        let str : string = (statementToString (Statement.Block ifExpr.Consequence))
        sb.Append str |> ignore

        if ifExpr.Alternative.IsSome then
            sb.Append "else" |> ignore

            sb.Append (statementToString (Statement.Block ifExpr.Alternative.Value))
            |> ignore
    | Func func ->
        let paramList =
            func.Parameters.Select (fun x -> expressionToString (Expression.Ident x))

        sb.Append func.Token.Literal |> ignore
        sb.Append "(" |> ignore
        sb.Append (String.Join (", ", paramList)) |> ignore
        sb.Append ") " |> ignore
        sb.Append (statementToString (Statement.Block func.Body)) |> ignore
    | Call call ->
        let args = call.Arguments.Select expressionToString
        sb.Append (expressionToString call.Function) |> ignore
        sb.Append "(" |> ignore
        sb.Append (String.Join (", ", args)) |> ignore
        sb.Append ")" |> ignore

    sb.ToString ()

and statementToString (stmt : Statement) =
    match stmt with
    | Let letStmt ->
        letStmt.Token.Literal
        + " "
        + (expressionToString (Expression.Ident letStmt.Name))
        + " = "
        + (expressionToString letStmt.Value)
        + ";"
    | Return returnStmt ->
        returnStmt.Token.Literal
        + " "
        + (expressionToString returnStmt.ReturnValue)
        + ";"
    | Expr exprStmt -> exprStmt.ChildExpr |> Option.map expressionToString |> Option.defaultValue ""
    | Block blockStmt ->
        let sb = StringBuilder ()

        for s in blockStmt.Statements do
            sb.AppendLine (statementToString s) |> ignore

        sb.ToString ()

let programToString (program : Program) =
    let builder = StringBuilder ()

    for s in program.Statements do
        builder.Append (statementToString s) |> ignore

    builder.ToString ()
