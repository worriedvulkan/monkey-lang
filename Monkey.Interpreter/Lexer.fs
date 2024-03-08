namespace Monkey.Interpreter

type Lexer =
    { Position : int
      ReadPosition : int
      Input : string
      Ch : char
      LastToken : Token }

module Lexer =

    open System

    let makeToken tt lit : Token = { Type = tt ; Literal = lit }

    let isLetter (ch : char) = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch = '_'

    let isDigit (ch : char) = ('0' <= ch && ch <= '9')

    let readChar (l : Lexer) =
        let ch =
            if l.ReadPosition >= l.Input.Length then '\x00' else l.Input[l.ReadPosition]

        { l with
            Ch = ch
            Position = l.ReadPosition
            ReadPosition = l.ReadPosition + 1 }

    let peekChar (l : Lexer) : char =
        if l.ReadPosition >= l.Input.Length then '\x00' else l.Input[l.ReadPosition]

    let rec loopUntil (predicate : char -> bool) (l : Lexer) : Lexer =
        if predicate l.Ch then loopUntil predicate (readChar l) else l

    let rec readIdentifier (l : Lexer) : string * Lexer =
        let position = l.Position
        let l = loopUntil isLetter l
        (l.Input.Substring (position, l.Position - position), l)

    let rec readNumber (l : Lexer) : string * Lexer =
        let position = l.Position
        let l = loopUntil isDigit l
        (l.Input.Substring (position, l.Position - position), l)

    let init (input : string) : Lexer =
        readChar
            { Ch = '\x00'
              Input = input
              Position = 0
              ReadPosition = 0
              LastToken = { Type = TokenType.Illegal ; Literal = "\x00" } }

    let rec skipWhitespace (l : Lexer) =
        if Char.IsWhiteSpace l.Ch then skipWhitespace (readChar l) else l

    let nextToken (l : Lexer) =
        let l = skipWhitespace l

        match l.Ch with
        | ',' -> readChar { l with LastToken = makeToken TokenType.Comma "," }
        | ';' -> readChar { l with LastToken = makeToken TokenType.SemiColon ";" }
        | '(' -> readChar { l with LastToken = makeToken TokenType.LeftParen "(" }
        | ')' -> readChar { l with LastToken = makeToken TokenType.RightParen ")" }
        | '{' -> readChar { l with LastToken = makeToken TokenType.LeftBrace "{" }
        | '}' -> readChar { l with LastToken = makeToken TokenType.RightBrace "}" }
        | '[' -> readChar { l with LastToken = makeToken TokenType.LeftBracket "[" }
        | ']' -> readChar { l with LastToken = makeToken TokenType.RightBracket "]" }
        | '=' ->
            if (peekChar l) = '=' then
                let l = readChar l
                readChar { l with LastToken = makeToken TokenType.Equal "==" }
            else
                readChar { l with LastToken = makeToken TokenType.Assign "=" }
        | '!' ->
            if (peekChar l) = '=' then
                let l = readChar l
                readChar { l with LastToken = makeToken TokenType.NotEqual "!=" }
            else
                readChar { l with LastToken = makeToken TokenType.Bang "!" }
        | '+' -> readChar { l with LastToken = makeToken TokenType.Add "+" }
        | '-' -> readChar { l with LastToken = makeToken TokenType.Minus "-" }
        | '*' -> readChar { l with LastToken = makeToken TokenType.Multiply "*" }
        | '/' -> readChar { l with LastToken = makeToken TokenType.Divide "/" }
        | '<' -> readChar { l with LastToken = makeToken TokenType.LessThan "<" }
        | '>' -> readChar { l with LastToken = makeToken TokenType.GreaterThan ">" }
        | '\x00' -> readChar { l with LastToken = makeToken TokenType.Eof "\x00" }
        | _ ->
            if isLetter l.Ch then
                let lit, l = readIdentifier l
                { l with LastToken = makeToken (Token.lookupIdent lit) lit }
            else if isDigit l.Ch then
                let num, l = readNumber l
                { l with LastToken = makeToken TokenType.Int num }
            else
                readChar
                    { l with LastToken = { Type = TokenType.Illegal ; Literal = l.Ch.ToString () } }
