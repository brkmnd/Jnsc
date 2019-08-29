module Jnsc
open System
open System.IO
open System.Text
open System.Collections.Generic
type Status =
    | LexerError of string
    | SyntaxError of string
    | TypeError of string
    | CompilerError of string
    | Success
type Result = {
    code:string
    status:Status
    warnings:string
    }
let private newResult code status =
    {code = code;status=status;warnings=""}
let private lex str =
    try (JanusLexer.lex str,"") with
    | Failure msg -> ([||],msg)
let private parse tokens lexbuf =
    let retval =
        try (JanusParser.start_entry (JanusLexer.getNextToken tokens) lexbuf,"") with
        | _ ->
            (JanusAbSyn.Prg.Error,JanusParser.ErrorContextDescriptor)
    JanusLexer.clearModule()
    retval
let private typeCheck tree =
    try (JanusTypeChecker.eval tree,"") with
    | Failure msg -> (tree,msg)
    | f ->
        let msg = sprintf "internal error [%A]" f
        (tree,msg)
let private comp2bob tree =
    try (JanusCompilerBob.compile tree,"") with
    | Failure msg -> (("",new List<string>()),msg)
let toBob str =
    let lexbuf = FSharp.Text.Lexing.LexBuffer<char>.FromString str
    match lex str with
    | (tokens,"") ->
        match parse tokens lexbuf with
        | (tree,"") ->
            match typeCheck tree with
            | (ttree,"") ->
                match comp2bob tree with
                | ((code,warns),"") -> newResult code Success
                | (_,msg) -> newResult ""  (CompilerError msg)
            | (_,msg) -> newResult "" (TypeError msg)
        | (_,msg) -> newResult "" (SyntaxError msg)
    | (_,msg) -> newResult "" (LexerError msg)
let hasSuccess = function
    | {code=_;status=Success;warnings=_} -> true
    | _ -> false
let hasError res = not (hasSuccess res)
let echoError res =
    match res.status with
    | LexerError msg -> sprintf "lexer-error: %s" msg
    | SyntaxError msg -> sprintf "syntax-error: %s" msg 
    | TypeError msg -> sprintf "type-error: %s" msg
    | CompilerError msg -> sprintf "comiler-error: %s" msg
    | _ -> "" 
