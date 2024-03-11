namespace Monkey.Interpreter

open System
open System.Collections.Generic

type OpPrecedence =
    | Lowest = 1
    | Equals = 2
    | LessGreater = 3
    | Sum = 4
    | Product = 5
    | Prefix = 6
    | Call = 7

type Parser = {
    Lexer : Lexer
    CurToken : Token
    PeekToken : Token
    Errors : List<string>
    PrefixParseFns : Map<TokenType, Parser -> Ast.Expression option * Parser>
    InfixParseFns : Map<TokenType, Ast.Expression -> Parser -> Ast.Expression option * Parser>
}

module Parser =

    let precedences =
        Map [
            TokenType.Equal, OpPrecedence.Equals
            TokenType.NotEqual, OpPrecedence.Equals
            TokenType.LessThan, OpPrecedence.LessGreater
            TokenType.GreaterThan, OpPrecedence.LessGreater
            TokenType.Add, OpPrecedence.Sum
            TokenType.Minus, OpPrecedence.Sum
            TokenType.Divide, OpPrecedence.Product
            TokenType.Multiply, OpPrecedence.Product
            TokenType.LeftParen, OpPrecedence.Call
        ]

    let curTokenIs (t : TokenType) (p : Parser) = p.CurToken.Type = t

    let peekTokenIs (t : TokenType) (p : Parser) = p.PeekToken.Type = t

    let peekPrecedence (p : Parser) =
        precedences.TryFind (p.PeekToken.Type)
        |> Option.defaultValue OpPrecedence.Lowest

    let curPrecedence (p : Parser) =
        precedences.TryFind (p.CurToken.Type) |> Option.defaultValue OpPrecedence.Lowest

    let nextToken (p : Parser) =
        let lexer = Lexer.nextToken p.Lexer

        { p with CurToken = p.PeekToken ; PeekToken = lexer.LastToken ; Lexer = lexer }

    let peekError (t : TokenType) (p : Parser) =
        p.Errors.Add ($"Expected next token to be %A{t}, got %A{p.PeekToken.Type} instead")

    let expectPeek (t : TokenType) (p : Parser) =
        if peekTokenIs t p then
            true, (nextToken p)
        else
            peekError t p
            false, p

    let rec loopUntil (tok : TokenType) (p : Parser) =
        if curTokenIs tok p then loopUntil tok (nextToken p) else p

    let parseExpression (precedence : OpPrecedence) (p : Parser) : Ast.Expression option * Parser =
        let prefix = p.PrefixParseFns.TryFind p.CurToken.Type

        match prefix with
        | None ->
            p.Errors.Add $"No Prefix Parse Functions for {p.CurToken.Type} found"
            None, p
        | Some fn ->
            let leftExpr, p = fn p

            let rec loopUntil (expr : Ast.Expression) (p : Parser) =
                if peekTokenIs TokenType.SemiColon p = false && precedence < peekPrecedence p then
                    match p.InfixParseFns.TryFind p.CurToken.Type with
                    | None -> expr, p
                    | Some fn ->
                        let p = nextToken p
                        let expr, p = fn expr p
                        loopUntil expr, p
                else
                    expr, p

            leftExpr, p

    let parseIdentifier (p : Parser) =
        Some (Ast.Expression.Ident { Token = p.CurToken ; Value = p.CurToken.Literal }), p

    let parseIntegerLiteral (p : Parser) =
        match Int32.TryParse p.CurToken.Literal with
        | true, x -> Some (Ast.Expression.Int { Token = p.CurToken ; Value = x }), p
        | false, _ ->
            p.Errors.Add ($"Could not parse %s{p.CurToken.Literal} as integer")
            None, p

    let parsePrefixExpression (p : Parser) =
        let tok = p.CurToken
        let op = p.CurToken.Literal
        let p = nextToken p
        let right, p = parseExpression OpPrecedence.Prefix p
        Some (Ast.Expression.Prefix { Token = tok ; Operator = op ; Right = right.Value }), p

    let parseInfixExpression (left : Ast.Expression) (p : Parser) =
        let tok = p.CurToken
        let op = p.CurToken.Literal
        let precedence = curPrecedence p
        let p = nextToken p
        let right, p = parseExpression precedence p

        let stmt =
            Ast.Expression.Infix { Token = tok ; Left = left ; Operator = op ; Right = right.Value }

        Some stmt, p

    let parseLetStatement (p : Parser) : Ast.Statement option * Parser =
        let tok = p.CurToken
        let ok, p = expectPeek TokenType.Ident p

        if ok = false then
            None, p
        else
            let name : Ast.Identifier = { Token = p.CurToken ; Value = p.CurToken.Literal }
            let ok, p = expectPeek TokenType.Assign p

            if ok = false then
                None, p
            else
                // TODO Do Later
                let p = loopUntil TokenType.SemiColon p

                let stmt =
                    Ast.Statement.Let { Token = tok ; Name = name ; Value = Ast.Expression.Empty }

                Some stmt, p

    let parseReturnStatement (p : Parser) : Ast.Statement option * Parser =
        let tok = p.CurToken
        let p = nextToken p
        // TODO Do Later
        let p = loopUntil TokenType.SemiColon p
        Some (Ast.Statement.Return { Token = tok ; ReturnValue = Ast.Expression.Empty }), p

    let parseExpressionStatement (p : Parser) : Ast.Statement option * Parser =
        let tok = p.CurToken
        let expr, p = parseExpression OpPrecedence.Lowest p
        let p = if peekTokenIs TokenType.SemiColon p then nextToken p else p
        Some (Ast.Statement.Expr { Token = tok ; ChildExpr = expr }), p

    let parseStatement (p : Parser) : Ast.Statement option * Parser =
        match p.CurToken.Type with
        | TokenType.Let -> parseLetStatement p
        | TokenType.Return -> parseReturnStatement p
        | _ -> None, p

    let init (l : Lexer) : Parser =
        let prefixMap =
            Map [
                TokenType.Ident, parseIdentifier
                TokenType.Int, parseIntegerLiteral
                TokenType.Bang, parsePrefixExpression
                TokenType.Minus, parsePrefixExpression
            ]

        let infixMap =
            Map [
                TokenType.Add, parseInfixExpression
                TokenType.Minus, parseInfixExpression
                TokenType.Divide, parseInfixExpression
                TokenType.Multiply, parseInfixExpression
                TokenType.Equal, parseInfixExpression
                TokenType.NotEqual, parseInfixExpression
                TokenType.LessThan, parseInfixExpression
                TokenType.GreaterThan, parseInfixExpression
            ]

        {
            Lexer = l
            CurToken = Lexer.makeToken TokenType.Illegal ""
            PeekToken = Lexer.makeToken TokenType.Illegal ""
            Errors = List<string> ()
            PrefixParseFns = prefixMap
            InfixParseFns = infixMap
        }
        |> nextToken
        |> nextToken

    let parseProgram (p : Parser) : Ast.Program * Parser =
        let stmt = List<Ast.Statement> ()
        let mutable p = p

        while p.CurToken.Type <> TokenType.Eof do
            let stmtOpt, parser = parseStatement p

            if stmtOpt.IsSome then
                stmt.Add stmtOpt.Value

            p <- nextToken parser

        { Statements = stmt }, p
