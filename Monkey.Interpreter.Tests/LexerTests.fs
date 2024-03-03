module Monkey.Interpreter.Tests.LexerTests

open NUnit.Framework
open Monkey.Interpreter

type private TestCase = { Input : string ; Expected : {| Tt : TokenType ; Literal : string |} list }

let private symbols = {
    Input = "=!+-*/(){}[]< > == != ,;"
    Expected = [
        {| Tt = TokenType.Assign; Literal = "=" |}
        {| Tt = TokenType.Bang; Literal = "!" |}
        {| Tt = TokenType.Add; Literal = "+" |}
        {| Tt = TokenType.Minus; Literal = "-" |}
        {| Tt = TokenType.Multiply; Literal = "*" |}
        {| Tt = TokenType.Divide; Literal = "/" |}
        {| Tt = TokenType.LeftParen; Literal = "(" |}
        {| Tt = TokenType.RightParen; Literal = ")" |}
        {| Tt = TokenType.LeftBrace; Literal = "{" |}
        {| Tt = TokenType.RightBrace; Literal = "}" |}
        {| Tt = TokenType.LeftBracket; Literal = "[" |}
        {| Tt = TokenType.RightBracket; Literal = "]" |}
        {| Tt = TokenType.LessThan; Literal = "<" |}
        {| Tt = TokenType.GreaterThan; Literal = ">" |}
        {| Tt = TokenType.Equal; Literal = "==" |}
        {| Tt = TokenType.NotEqual; Literal = "!=" |}
        {| Tt = TokenType.Comma; Literal = "," |}
        {| Tt = TokenType.SemiColon; Literal = ";" |}
        {| Tt = TokenType.Eof; Literal = "\x00" |}
    ] 
}

let private letDeclaration = {
    Input = "let five = 5;"
    Expected = [
        {| Tt = TokenType.Let; Literal = "let" |}
        {| Tt = TokenType.Ident; Literal = "five" |}
        {| Tt = TokenType.Assign; Literal = "=" |}
        {| Tt = TokenType.Int; Literal = "5" |}
        {| Tt = TokenType.SemiColon; Literal = ";" |}
        {| Tt = TokenType.Eof; Literal = "\x00" |}
    ] 
}

let private functionDeclaration = {
    Input = "let add = fn(x, y) { x + y; };"
    Expected = [
        {| Tt = TokenType.Let; Literal = "let" |}
        {| Tt = TokenType.Ident; Literal = "add" |}
        {| Tt = TokenType.Assign; Literal = "=" |}
        {| Tt = TokenType.Function; Literal = "fn" |}
        {| Tt = TokenType.LeftParen; Literal = "(" |}
        {| Tt = TokenType.Ident; Literal = "x" |}
        {| Tt = TokenType.Comma; Literal = "," |}
        {| Tt = TokenType.Ident; Literal = "y" |}
        {| Tt = TokenType.RightParen; Literal = ")" |}
        {| Tt = TokenType.LeftBrace; Literal = "{" |}
        {| Tt = TokenType.Ident; Literal = "x" |}
        {| Tt = TokenType.Add; Literal = "+" |}
        {| Tt = TokenType.Ident; Literal = "y" |}
        {| Tt = TokenType.SemiColon; Literal = ";" |}
        {| Tt = TokenType.RightBrace; Literal = "}" |}
        {| Tt = TokenType.SemiColon; Literal = ";" |}
        {| Tt = TokenType.Eof; Literal = "\x00" |}
    ] 
}

let private ifDeclaration = {
    Input = "if (5 < 10) { return true; } else { return false; }"
    Expected = [
        {| Tt = TokenType.If; Literal = "if" |}
        {| Tt = TokenType.LeftParen; Literal = "(" |}
        {| Tt = TokenType.Int; Literal = "5" |}
        {| Tt = TokenType.LessThan; Literal = "<" |}
        {| Tt = TokenType.Int; Literal = "10" |}
        {| Tt = TokenType.RightParen; Literal = ")" |}
        {| Tt = TokenType.LeftBrace; Literal = "{" |}
        {| Tt = TokenType.Return; Literal = "return" |}
        {| Tt = TokenType.True; Literal = "true" |}
        {| Tt = TokenType.SemiColon; Literal = ";" |}
        {| Tt = TokenType.RightBrace; Literal = "}" |}
        {| Tt = TokenType.Else; Literal = "else" |}
        {| Tt = TokenType.LeftBrace; Literal = "{" |}
        {| Tt = TokenType.Return; Literal = "return" |}
        {| Tt = TokenType.False; Literal = "false" |}
        {| Tt = TokenType.SemiColon; Literal = ";" |}
        {| Tt = TokenType.RightBrace; Literal = "}" |}
        {| Tt = TokenType.Eof; Literal = "\x00" |}
    ] 
}

[<Test>]
let ``Next Token`` () =
    let testCases = [symbols ; letDeclaration; functionDeclaration; ifDeclaration]

    for bag in testCases do
        let mutable lexer = Lexer.init bag.Input

        for i = 0 to bag.Expected.Length - 1 do
            lexer <- Lexer.nextToken lexer
            let expected = bag.Expected[i]

            Assert.AreEqual (
                expected.Tt,
                lexer.LastToken.Type,
                "Tests{0} - TokenType Wrong. Expected={1}, Got={2}",
                i,
                expected.Tt,
                lexer.LastToken.Type
            )
            
            Assert.AreEqual (
                expected.Literal,
                lexer.LastToken.Literal,
                "Tests{0} - Literal Wrong. Expected={1}, Got={2}",
                i,
                expected.Literal,
                lexer.LastToken.Literal
            )
