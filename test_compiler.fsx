#r "FsLexYacc.Runtime.9.0.2/lib/net46/FsLexYacc.Runtime.dll"
#load "JanusAbSyn.fs"
#load "JanusParser.fs"
#load "JanusLexer.fs"
#load "JanusTypeChecker.fs"
#load "Janus2Bob.fs"
open System
open System.IO
open System.Text
open System.Collections.Generic

let relpath = "tests_janus/"
let printfn_color str c =
    let stdc = Console.ForegroundColor
    Console.ForegroundColor <- c
    printfn "%s" str
    Console.ForegroundColor <- stdc
type Result () =
    let res = new Dictionary<string,int>()
    member this.contains(k) = res.ContainsKey(k)
    member this.get(k) = res.[k]
    member this.add (k,v) =
        if res.ContainsKey (k) then res.[k] <- v
        else res.Add(k,v)
    member this.fromFile (name) =
        let fname = sprintf "%s.res" name
        let ls = 
            try System.IO.File.ReadAllLines (relpath + fname) with
            | _ -> [||]
        for l in ls do
            let lref = l.Replace(" ","")
            let a = lref.Split ('=')
            if a.Length = 2 then
                let k = a.[0]
                let v = (int) a.[1]
                this.add(k,v)
            else ()
    member this.len () = res.Count
    member this.foreach(f) =
        for r in res do
            f r.Key r.Value
    member this.toString () =
        let mutable retval = ""
        this.foreach(fun k v -> retval <- retval + (sprintf "%s = %d\n" k v))
        if retval.Length > 0 then retval.[0 .. retval.Length - 2]
        else retval
    with
    static member (==) (res1 : Result,res2 : Result) =
        if res1.len() <> res2.len() then false
        else
            let mutable retval = true
            let f k1 v1 =
                if not retval then ()
                elif not (res2.contains (k1)) then retval <- false
                else retval <- v1 = res2.get (k1)
            res1.foreach(f)
            retval
    static member (!=) (res1 : Result,res2 : Result) = not (res1 == res2)
let fopen name =
    let fname = sprintf "%s.jns" name
    let l,msg =
        try (System.IO.File.ReadAllLines (relpath + fname),"") with
        | _ -> ([||],sprintf "file '%s' not found" fname)
    let res =
        Array.fold
            (fun acc line -> acc + line + "\n")
            ""
            l
    (res,msg)
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
    if compileMsg <> "" then
        printfn_color compileMsg ConsoleColor.Red
        ""
    else
        for w in warns do
            printfn "compiler warning: %s" w
        bobcode

module Html =
    let writeOut txt =
        let path = "sim.html"
        File.WriteAllText (path,txt)
    let newStyle () =
        let styles =
            ".bobprg{display:none;font-family:monospace;white-space:pre}"+
            ".error-text{color:red}"
        sprintf "<style>%s</style>" styles
    let newScripts () =
        "<script src='BobSim/stack.js'></script>"+
        "<script src='BobSim/machine.js'></script>"+
        "<script src='BobSim/bob.js'></script>"+
        "<script src='BobSim/run.js'></script>"
    let newDoc body =
        "<!DOCTYPE html>\n"+
        "<html>\n"+
        "<head>"+newStyle()+"</head>\n"+
        "<body>\n"+
        body+"\n"+
        newScripts()+"\n"+
        "</body>\n"+
        "</html>"
    let newPout1 t = sprintf "<p id='out1'>%s</p>" t
    let newPerror t = sprintf "<p class='error-text'>%s</p>" t
    let newH2 t = sprintf "<h2>%s</h2>" t
    let newBobCode c = sprintf "<span class='bobprg'>%s</span>" c

            
[<EntryPoint>]
let main args =
    let len = Array.length args
    printfn "\n"
    if Array.length args <> 1 then
        printfn "error in number of arguments given to test, expected 1, given %d" len
        0
    else
        let prg = args.[0]
        let src,smsg = fopen prg
        let expct =
            let e = new Result()
            e.fromFile (prg)
            e
        let res =
            Html.newH2 (sprintf "testing '%s'\n" prg) +
            if smsg <> "" then Html.newPerror smsg
            else Html.newBobCode (compileJanus src)
        Html.writeOut (Html.newDoc (res + Html.newPout1 ""))
        1
