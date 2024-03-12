namespace Monkey.Interpreter

open System
open System.Collections.Generic

type ParserClass(_lexer : Lexer) =
    let mutable lexer = _lexer
    let mutable curToken : Token = Lexer.makeToken TokenType.Illegal ""
    let mutable peekToken : Token = Lexer.makeToken TokenType.Illegal ""
    let prefixParseFns = Map []
    let infixParseFns = Map []
    let errors = List<string> ()

    let precedenceTable =
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

    let nextToken () =
        lexer <- Lexer.nextToken lexer
        curToken <- peekToken
        peekToken <- lexer.LastToken

    do
        nextToken ()
        nextToken ()

    let curTokenIs (t : TokenType) = curToken.Type = t

    let peekTokenIs (t : TokenType) = peekToken.Type = t

    let peekPrecedence () =
        precedenceTable.TryFind peekToken.Type
        |> Option.defaultValue OpPrecedence.Lowest

    let curPrecedence () =
        precedenceTable.TryFind curToken.Type |> Option.defaultValue OpPrecedence.Lowest

    let peekError (t : TokenType) =
        errors.Add $"Expected next token to be %A{t}, got %A{peekToken.Type} instead"

    let expectPeek (t : TokenType) =
        if peekTokenIs t then
            nextToken ()
            true
        else
            peekError t
            false

    let parseExpression (precedence : OpPrecedence) : Ast.Expression option =
        let prefix = prefixParseFns.TryFind curToken.Type

        match prefix with
        | None ->
            errors.Add $"No Prefix Parse Functions for {curToken.Type} found"
            None
        | Some fn ->
            let mutable leftExpr = fn ()
            let mutable running = true

            while peekTokenIs TokenType.SemiColon = false
                  && precedence < peekPrecedence ()
                  && running do
                match infixParseFns.TryFind peekToken.Type with
                | None -> ()
                | Some infixfn ->
                    nextToken ()
                    leftExpr <- infixfn leftExpr

            leftExpr

    let parseIdentifier () =
        Some (Ast.Expression.Ident { Token = curToken ; Value = curToken.Literal })

    let parseIntegerLiteral () =
        match Int32.TryParse curToken.Literal with
        | true, x -> Some (Ast.Expression.Int { Token = curToken ; Value = x })
        | false, _ ->
            errors.Add $"Could not parse %s{curToken.Literal} as integer"
            None
    
    let parseBoolean () =
        Some (Ast.Expression.Bool {Token = curToken ; Value = curTokenIs TokenType.True })
        
    let parseGroupedExpression () =
        nextToken()
        let expr = parseExpression OpPrecedence.Lowest
        if expectPeek TokenType.RightParen = false then
            None
        else
            expr
            
    let parsePrefixExpression () =
        let tok = curToken
        let op = curToken.Literal
        nextToken()
        let right = parseExpression OpPrecedence.Prefix
        Some (Ast.Expression.Prefix { Token = tok ; Operator = op ; Right = right.Value })
            
    let parseInfixExpression (left : Ast.Expression) =
        let tok = curToken
        let op = curToken.Literal
        let precedence = curPrecedence ()
        nextToken ()
        let right = parseExpression precedence

        Some (
            Ast.Expression.Infix { Token = tok ; Left = left ; Operator = op ; Right = right.Value }
        )
    
    let parseLetStatement () =
        let tok = curToken
        if expectPeek TokenType.Ident = false then
            None
        else
            let name : Ast.Identifier = {Token = curToken; Value = curToken.Literal}
            if expectPeek TokenType.Assign = false then
                None
            else
                nextToken()
                let value = parseExpression OpPrecedence.Lowest
                if peekTokenIs TokenType.SemiColon then
                    nextToken()
                
                Some (Ast.Statement.Let {Token = tok ; Value = value.Value ; Name = name})
            
    let parseReturnStatement () =
        let tok = curToken
        nextToken()
        let returnValue = parseExpression OpPrecedence.Lowest
        if peekTokenIs TokenType.SemiColon then
            nextToken()
        Some (Ast.Statement.Return {Token = tok ; ReturnValue = returnValue.Value })
    
    let parseExpressionStatement () =
        let tok = curToken
        let expr = parseExpression OpPrecedence.Lowest
        if peekTokenIs TokenType.SemiColon then
            nextToken()
        Some (Ast.Statement.Expr {Token = tok ; ChildExpr = expr })
    
    let parseStatement () =
        match curToken.Type with
        | TokenType.Let -> parseLetStatement()
        | TokenType.Return -> parseReturnStatement()
        | _ -> parseExpressionStatement()
    
    let parseBlockStatement () =
        let tok = curToken
        let stmts = List<Ast.Statement>()
        nextToken()
        while curTokenIs TokenType.RightBrace = false && curTokenIs TokenType.Eof = false do
            let stmt = parseStatement()
            if stmt.IsSome then
                stmts.Add(stmt.Value)
            nextToken()
        Some (Ast.Statement.Block {Token = tok ; Statements = stmts })
            
    let parseIfExpression () =
        let tok = curToken
        if expectPeek TokenType.LeftParen = false then
            None
        else
            nextToken()
            let cond = parseExpression OpPrecedence.Lowest
            if expectPeek TokenType.RightParen = false then
                None
            elif expectPeek TokenType.LeftBrace = false then
                None
            else
                let cons = parseBlockStatement()
                if peekTokenIs TokenType.Else then
                    nextToken()
                    if expectPeek TokenType.LeftBrace = false then
                        None
                    else
                        Some (Ast.Expression.If {Token = tok ; Condition = cond.Value ; Consequence = cons ; Alternative = parseBlockStatement()})
                else
                    Some (Ast.Expression.If {Token = tok ; Condition = cond.Value ; Consequence = cons ; Alternative = None})
                    
    let parseCallArguments () =
        let args = List<Ast.Expression>()
        if peekTokenIs TokenType.RightParen then
            nextToken()
            args
        else
            nextToken()
            args.Add((parseExpression OpPrecedence.Lowest).Value)
            while peekTokenIs TokenType.Comma do
                nextToken()
                nextToken()
                args.Add((parseExpression OpPrecedence.Lowest).Value)
            if expectPeek TokenType.RightParen = false then
                List<Ast.Expression>()
            else args
            
    let parseCallExpression (expr : Ast.Expression) =
        Some (Ast.Expression.Call {Token = curToken ; Function = expr ; Arguments = parseCallArguments() })
        
    let parseFunctionParameters () =
        let identifiers = List<Ast.Identifier>()
        
        if peekTokenIs(TokenType.RightParen) then
            nextToken()
            identifiers
        else
            nextToken()
            identifiers.Add {Token = curToken ; Value = curToken.Literal }
            while peekTokenIs TokenType.Comma do
                nextToken()
                nextToken()
                identifiers.Add {Token = curToken ; Value = curToken.Literal }
            
            if expectPeek TokenType.RightParen = false then
                identifiers.Clear()
            
            identifiers
            
    let parseFunctionLiteral () =
        let tok = curToken
        if expectPeek TokenType.LeftParen = false then
            None
        else
            let parameters = parseFunctionParameters ()
            if expectPeek TokenType.LeftParen = false then
                None
            else
                let body = parseBlockStatement()
                Some (Ast.Expression.Func {Token = tok ; Parameters = parameters ; Body = Ast.BlockStmt body.Value })