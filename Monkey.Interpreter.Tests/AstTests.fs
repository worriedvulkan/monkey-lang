module Monkey.Interpreter.Tests.AstTests

open NUnit.Framework
open Monkey.Interpreter
open System.Collections.Generic

[<Test>]
let ``Program to String`` () =
    let stmts = List<Ast.Statement> ()

    stmts.Add (
        Ast.Statement.Let
            {
                Token = { Type = TokenType.Let ; Literal = "let" }
                Name = { Token = { Type = TokenType.Ident ; Literal = "myVar" } ; Value = "myVar" }
                Value =
                    Ast.Expression.Identifier {
                        Token = { Type = TokenType.Ident ; Literal = "anotherVar" }
                        Value = "anotherVar"
                    }
            }
    )

    let program : Ast.Program = { Statements = stmts }
    Assert.AreEqual(Ast.programToString program, "let myVar = anotherVar;")
