//Interface for the Janus2Bob compiler
let compileJanus str =
    // on success return bobcode
    // on error print error and return empty string
    let lexbuf = FSharp.Text.Lexing.LexBuffer<char>.FromString str
    let tokens,lexerMsg =
        try (JanusLexer.lex str,"") with
        | Failure msg -> ([||],msg)
    let tree,parserMsg =
        if lexerMsg <> "" then
            let msg = sprintf "lexer-error: %s\ntokens: %A" lexerMsg tokens
            (JanusAbSyn.Prg.Error,msg)
        else
            try (JanusParser.start_entry (JanusLexer.getNextToken tokens) lexbuf,"") with
            | _ ->
                let msg = sprintf "syntax-error: %s" JanusParser.ErrorContextDescriptor
                (JanusAbSyn.Prg.Error,msg)
    let typedTree,typeMsg =
        if parserMsg <> "" then (tree,parserMsg)
        else
            try (JanusTypeChecker.eval tree,"") with
            | JanusTypeChecker.JanusTypeError tmsg ->
                let msg = sprintf "type-error %s" tmsg
                (tree,msg)
            | f ->
                let msg = sprintf "internal error in typechecker : %A" f
                (tree,msg)
    let (bobcode,warns),compileMsg =
        if typeMsg <> "" then (("",new List<string>()),typeMsg)
        else
            try (Janus2Bob.compile typedTree,"") with
            | Failure msg -> (("",new List<string>()),msg)
    // clean-up
    JanusLexer.clearModule()
    if compileMsg <> "" then
        printfn_color compileMsg ConsoleColor.Red
        ""
    else
        for w in warns do
            printfn "compiler warning: %s" w
        bobcode
