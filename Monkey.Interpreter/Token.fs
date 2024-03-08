namespace Monkey.Interpreter

[<RequireQualifiedAccess>]
type TokenType =
    | Illegal
    | Eof
    // Operators
    | Assign
    | Add
    | Minus
    | Multiply
    | Divide
    | GreaterThan
    | EqualOrGreaterThan
    | LessThan
    | EqualOrLessThan
    | Equal
    | NotEqual
    | Bang
    | Comma
    | SemiColon
    | Colon
    // Identifiers + Literals + Keywords
    | Let
    | Ident
    | Int
    | String
    | Function
    | True
    | False
    | If
    | Else
    | For
    | Return
    // Brackets
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | LeftBracket
    | RightBracket
    

type Token = {Type: TokenType; Literal: string} 
    with
        override this.ToString () = 
            $"{{Type: {this.Type}; Literal: '{this.Literal}' }}"

module Token =
    let private keywords = Map [
        "fn", TokenType.Function
        "let", TokenType.Let
        "if", TokenType.If
        "else", TokenType.Else
        "true", TokenType.True
        "false", TokenType.False
        "return", TokenType.Return
    ]
    
    let lookupIdent (ident: string): TokenType =
        Map.tryFind ident keywords
        |> Option.defaultValue TokenType.Ident