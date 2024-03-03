namespace Monkey.Interpreter

open System.Collections.Generic

type Parser = { Lexer : Lexer ; CurToken : Token ; PeekToken : Token ; Errors : List<string> }

module Parser =

    let curTokenIs (t : TokenType) (p : Parser) = p.CurToken.Type = t

    let peekTokenIs (t : TokenType) (p : Parser) = p.PeekToken.Type = t

    let nextToken (p : Parser) =
        { p with
            CurToken = p.PeekToken
            PeekToken = (Lexer.nextToken p.Lexer).LastToken
        }

    let init (l : Lexer) : Parser =
        {
            Lexer = l
            CurToken = Lexer.makeToken TokenType.Illegal ""
            PeekToken = Lexer.makeToken TokenType.Illegal ""
            Errors = List<string> ()
        }
        |> nextToken
        |> nextToken

    let expectPeek (t : TokenType) (p : Parser) =
        if peekTokenIs t p then true, (nextToken p) else false, (nextToken p)

    let peekError (t : TokenType) (p : Parser) =
        p.Errors.Add ($"Expected next token to be %A{t}, got %A{p.PeekToken.Type} instead")

    let rec loopUntil (tok : TokenType) (p : Parser) = if curTokenIs tok p then nextToken p else p

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

                (Some (
                    Ast.Statement.Let { Token = tok ; Name = name ; Value = Ast.Expression.Empty }
                 ),
                 p)

    let parseReturnStatement (p : Parser) : Ast.Statement option * Parser =
        let tok = p.CurToken
        let p = nextToken p
        // TODO Do Later
        let p = loopUntil TokenType.SemiColon p
        Some (Ast.Statement.Return { Token = tok ; ReturnValue = Ast.Expression.Empty }), p

    let parseStatement (p : Parser) : Ast.Statement option * Parser =
        match p.CurToken.Type with
        | TokenType.Let -> parseLetStatement p
        | TokenType.Return -> parseReturnStatement p
        | _ -> None, p

    let parseProgram (p : Parser) : Ast.Program * Parser =
        let stmt = List<Ast.Statement> ()
        let mutable p = p

        while p.CurToken.Type <> TokenType.Eof do
            let stmtOpt, parser = parseStatement p

            if stmtOpt.IsSome then
                stmt.Add stmtOpt.Value

            p <- nextToken parser

        { Statements = stmt }, p
