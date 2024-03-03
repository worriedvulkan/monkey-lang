open System
open Monkey.Interpreter

let rec repl () =
    Console.Write ">> "
    let line = Console.ReadLine ()

    if String.IsNullOrWhiteSpace line then
        ()
    else
        let mutable lexer = Lexer.init line
        let mutable running = true

        while running do
            lexer <- Lexer.nextToken lexer
            Console.WriteLine lexer.LastToken

            if lexer.LastToken.Type = TokenType.Eof then
                running <- false

        repl ()

Console.WriteLine "Welcome to the Monkey Programming Language"
Console.WriteLine "Feel free to type in commands"
repl ()

// TODO implementing Pratt parser