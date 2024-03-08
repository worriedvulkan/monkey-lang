module Monkey.Interpreter.Tests.ParserTests

open NUnit.Framework
open NUnit.Framework.Legacy
open Monkey.Interpreter

type private ExpectedContainer1<'a> = { Input : string ; Expected : 'a }

type private Prefix<'a> =
    struct
        val Input : string
        val Operator : string
        val Value : 'a
        new(input, op, value) = { Input = input ; Operator = op ; Value = value }
    end


type private Infix<'a, 'b> =
    struct
        val Input : string
        val LeftValue : 'a
        val Operator : string
        val RightValue : 'b

        new(input, left, op, right) =
            { Input = input ; LeftValue = left ; Operator = op ; RightValue = right }
    end

let testLetStatement (s : Ast.Statement) (name : string) =
    match s with
    | Ast.Statement.Let letStmt ->
        ClassicAssert.AreEqual (
            letStmt.Name.Value,
            name,
            "LetStmt.Name.Value not '{0}'. Got={1}",
            name,
            letStmt.Name.Value
        )

        ClassicAssert.AreEqual (
            Ast.statementTokenLiteral s,
            name,
            "LetStmt.Name.Value not '{0}'. Got={1}",
            name,
            Ast.statementTokenLiteral s
        )
    | _ -> Assert.Fail $"Statement is not a Let Statement. Got={Ast.statementTokenLiteral s}"

let testIntegerLiteral (expr : Ast.Expression) (value : int) =
    match expr with
    | Ast.Expression.Int il ->
        ClassicAssert.AreEqual (5, il.Value, "IntegerLiteral.Value not foobar. Got={0}", il.Value)

        ClassicAssert.AreEqual (
            "5",
            il.Token.Literal,
            "Int.Token.Literal not foobar. Got={0}",
            il.Token.Literal
        )
    | _ -> Assert.Fail $"Expression is not IntLiteral. Got={expr}"

let checkParserErrors (p : Parser) =
    match p.Errors.Count with
    | 0 -> ()
    | _ ->
        Assert.Multiple (fun _ ->
            Assert.Fail $"Parser has {p.Errors.Count} errors"

            for err in p.Errors do
                Assert.Fail $"Parser Error: {err}"
        )

[<Test>]
[<Parallelizable>]
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
            Assert.Fail
                $"Program.Statements does not contain 1 statement. Got={program.Statements.Count}"

        testLetStatement program.Statements[0] bag.Expected

[<Test>]
[<Parallelizable>]
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
            Assert.Fail
                $"Program.Statements does not contain 1 statement. Got={program.Statements.Count}"

        match program.Statements[0] with
        | Ast.Statement.Return returnStmt ->
            Assert.Fail
                $"Ast.Expression.Return.TokenLiteral not 'return'. Got={returnStmt.Token.Literal}"
        | stmt ->
            Assert.Fail ($"Stmt is not Ast.Expression.Return. Got={Ast.statementTokenLiteral stmt}")

[<Test>]
[<Parallelizable>]
let ``Identifier Expression`` () =
    let input = "foobar;"
    let lexer = Lexer.init input
    let parser = Parser.init lexer
    let program, parser = Parser.parseProgram parser
    checkParserErrors parser

    if program.Statements.Count <> 1 then
        Assert.Fail
            $"Program.Statements does not contain 1 statement. Got={program.Statements.Count}"

    let (Ast.Statement.Expr exprStmt) = program.Statements[0]
    let (Ast.Expression.Ident ident) = exprStmt.ChildExpr.Value
    ClassicAssert.AreEqual ("foobar", ident.Value, "Ident.Value not foobar. Got={0}", ident.Value)

    ClassicAssert.AreEqual (
        "foobar",
        ident.Token.Literal,
        "Ident.Token.Literal not foobar. Got={0}",
        ident.Token.Literal
    )

[<Test>]
[<Parallelizable>]
let ``Integer Literal Expression`` () =
    let input = "5;"
    let lexer = Lexer.init input
    let parser = Parser.init lexer
    let program, parser = Parser.parseProgram parser
    checkParserErrors parser

    if program.Statements.Count <> 1 then
        Assert.Fail
            $"Program.Statements does not contain 1 statement. Got={program.Statements.Count}"

    let (Ast.Statement.Expr exprStmt) = program.Statements[0]
    testIntegerLiteral exprStmt.ChildExpr.Value 5

[<Test>]
[<Parallelizable>]
let ``Parsing Prefix Expression`` () =
    let testCases = [ Prefix ("!5", "!", 5) ; Prefix ("-15", "-", 5) ]

    for test in testCases do
        let lexer = Lexer.init test.Input
        let parser = Parser.init lexer
        let program, parser = Parser.parseProgram parser
        checkParserErrors parser

        if program.Statements.Count <> 1 then
            Assert.Fail
                $"Program.Statements does not contain 1 statement. Got={program.Statements.Count}"

        let (Ast.Statement.Expr exprStmt) = program.Statements[0]
        let (Ast.Expression.Prefix prefix) = exprStmt.ChildExpr.Value

        ClassicAssert.AreEqual (
            test.Operator,
            prefix.Operator,
            "Expression.Operator is not '{0}'. Got={1}",
            test.Operator,
            prefix.Operator
        )

        testIntegerLiteral prefix.Right test.Value

[<Test>]
[<Parallelizable>]
let ``Parsing Infix Expression`` () =
    let testCases = [
        Infix ("5 + 5", 5, "+", 5)
        Infix ("5 - 5", 5, "-", 5)
        Infix ("5 * 5", 5, "*", 5)
        Infix ("5 / 5", 5, "/", 5)
        Infix ("5 > 5", 5, ">", 5)
        Infix ("5 < 5", 5, "<", 5)
        Infix ("5 == 5", 5, "==", 5)
        Infix ("5 != 5", 5, "!=", 5)
    ]

    for test in testCases do
        let lexer = Lexer.init test.Input
        let parser = Parser.init lexer
        let program, parser = Parser.parseProgram parser
        checkParserErrors parser

        if program.Statements.Count <> 1 then
            Assert.Fail
                $"Program.Statements does not contain 1 statement. Got={program.Statements.Count}"

        let (Ast.Statement.Expr exprStmt) = program.Statements[0]
        let (Ast.Expression.Infix infix) = exprStmt.ChildExpr.Value
        testIntegerLiteral infix.Left test.LeftValue

        ClassicAssert.AreEqual (
            test.Operator,
            infix.Operator,
            "Expr.Operator is not '{0}'. Got={1}",
            test.Operator,
            infix.Operator
        )

        testIntegerLiteral infix.Right test.RightValue
