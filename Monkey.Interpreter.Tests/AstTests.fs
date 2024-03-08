module Monkey.Interpreter.Tests.AstTests

open NUnit.Framework
open NUnit.Framework.Legacy
open Monkey.Interpreter
open System.Collections.Generic

[<Test>]
[<Parallelizable>]
let ``Program to String`` () =
    let stmts = List<Ast.Statement> ()

    stmts.Add (
        Ast.Statement.Let
            {
                Token = { Type = TokenType.Let ; Literal = "let" }
                Name = { Token = { Type = TokenType.Ident ; Literal = "myVar" } ; Value = "myVar" }
                Value =
                    Ast.Expression.Ident {
                        Token = { Type = TokenType.Ident ; Literal = "anotherVar" }
                        Value = "anotherVar"
                    }
            }
    )

    let program : Ast.Program = { Statements = stmts }
    ClassicAssert.AreEqual (Ast.programToString program, "let myVar = anotherVar;")
