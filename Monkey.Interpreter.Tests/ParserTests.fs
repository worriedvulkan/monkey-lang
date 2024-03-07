module Monkey.Interpreter.Tests.ParserTests

open NUnit.Framework
open Monkey.Interpreter

type private ExpectedContainer1<'a> = { Input : string ; Expected : 'a }

let testLetStatement (s : Ast.Statement) (name : string) =
    match s with
    | Ast.Statement.Let letStmt ->
        Assert.AreEqual (
            letStmt.Name.Value,
            name,
            "LetStmt.Name.Value not '{0}'. Got={1}",
            name,
            letStmt.Name.Value
        )

        Assert.AreEqual (
            (Ast.statementTokenLiteral s),
            name,
            "LetStmt.Name.Value not '{0}'. Got={1}",
            name,
            Ast.statementTokenLiteral s
        )
    | _ -> Assert.Fail ("Statement is not a Let Statement. Got={}", Ast.statementTokenLiteral s)

let checkParserErrors (p : Parser) =
    match p.Errors.Count with
    | 0 -> ()
    | _ ->
        Assert.Multiple (fun _ ->
            Assert.Fail ("Parser has {0} errors", p.Errors.Count)

            for err in p.Errors do
                Assert.Fail ("Parser Error: {0}", err)
        )

[<Test>]
let ``Let Statements`` () =
    let tests = [
        { Input = "let x = 5;" ; Expected = "x" }
        { Input = "let y = 50;" ; Expected = "y" }
        { Input = "let foobar = 500;" ; Expected = "foobar" }
    ]

    for bag in tests do
        let lexer = Lexer.init bag.Input
        let parser = Parser.init lexer
        let program, parser = Parser.parseProgram parser
        checkParserErrors parser

        if program.Statements.Count <> 1 then
            Assert.Fail (
                "Program.Statements does not contain 1 statement. Got={0}",
                program.Statements.Count
            )

        testLetStatement program.Statements[0] bag.Expected

[<Test>]
let ``Return Statements`` () =
    let tests = [
        { Input = "return 5;" ; Expected = "5" }
        { Input = "return 50;" ; Expected = "50" }
        { Input = "return 500;" ; Expected = "500" }
    ]

    for bag in tests do
        let lexer = Lexer.init bag.Input
        let parser = Parser.init lexer
        let program, parser = Parser.parseProgram parser
        checkParserErrors parser

        if program.Statements.Count <> 1 then
            Assert.Fail (
                "Program.Statements does not contain 1 statement. Got={0}",
                program.Statements.Count
            )

        match program.Statements[0] with
        | Ast.Statement.Return returnStmt ->
            Assert.Fail (
                "Ast.Expression.Return.TokenLiteral not 'return'. Got={0}",
                returnStmt.Token.Literal
            )
        | stmt ->
            Assert.Fail (
                "Stmt is not Ast.Expression.Return. Got={0}",
                Ast.statementTokenLiteral stmt
            )

[<Test>]
let ``Identifier Expression`` () =
    let input = "foobar;"
    let lexer = Lexer.init input
    let parser = Parser.init lexer
    let program, parser = Parser.parseProgram parser
    checkParserErrors parser

    if program.Statements.Count <> 1 then
        Assert.Fail (
            "Program.Statements does not contain 1 statement. Got={0}",
            program.Statements.Count
        )
    
    let (Ast.Statement.Expression exprStmt) = program.Statements[0]
    ()
    