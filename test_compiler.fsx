#r "FsLexYacc.Runtime.9.0.2/lib/net46/FsLexYacc.Runtime.dll"
#load "JanusAbSyn.fs"
#load "JanusParser.fs"
#load "JanusLexer.fs"
#load "JanusTypeChecker.fs"
#load "Janus2Bob.fs"
#load "Jnsc.fs"
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
        let mutable retval = "{"
        this.foreach(fun k v -> retval <- retval + (sprintf "\"%s\":%d," k v))
        if retval.Length > 0 then retval.[0 .. retval.Length - 2] + "}"
        else retval + "}"
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
    let res = Jnsc.compile str
    if res|>Jnsc.hasSuccess then
        res.code
    else
        printfn "%s" (res|>Jnsc.echoError)
        ""
module Html =
    let mutable code = ""
    let newStyle () =
        let styles =
            ".bobprg{display:none;font-family:monospace;white-space:pre}"+
            ".error-out{color:red}"+
            ".out{font-family:monospace;font-size:12pt;white-space:pre}"+
            "h2{color:blue}"+
            ".codetable{"+
            "display:block;width:100%;height:300px;background-color:black;"+
            "color:white;font-family:monospace;font-size:12pt;overflow-y:scroll;"+
            "}"
        sprintf "<style>%s</style>" styles
    let newHeadScripts () =
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
        newHeadScripts()+"\n"+
        "</body>\n"+
        "</html>"
    let newScript s = sprintf "<script>%s</script>" s
    let newPout t = sprintf "<p class='out'>%s</p>" t
    let newPerror t = sprintf "<p class='error-out'>%s</p>" t
    let newH1 t = sprintf "<h1>%s</h1>" t
    let newH2 t = sprintf "<h2>%s</h2>" t
    let newCodeBox t = sprintf "<table class='codetable'>%s</table>" t
    let newBobCode c = sprintf "<span class='bobprg'>%s</span>" c
    // For dealing with res
    let addCodeLineTop t = code <- sprintf "%s\n%s" t code
    let addCodeLine t =
        code <- sprintf "%s%s\n" code t
    let clearCode () = code <- ""
    let toString() = code
    let writeToSim () =
        let path = "sim.html"
        File.WriteAllText (path,newDoc code)

[<EntryPoint>]
let main args =
    let len = Array.length args
    if Array.length args = 0 then
        printfn "error in number of arguments given to test, expected 1, given %d" len
        0
    else
        let mutable results = ""
        Html.addCodeLine (Html.newH1 "Testing Janus2Bob")
        for prg in args do
            let src,smsg = fopen prg
            let expct =
                let e = new Result()
                e.fromFile (prg)
                e
            if smsg <> "" then printfn_color smsg ConsoleColor.Red
            else
                let res = new Result()
                res.fromFile(prg)
                results <- results + res.toString() + ","
                Html.addCodeLine (Html.newH2 (sprintf "%s" prg))
                Html.addCodeLine (Html.newBobCode (compileJanus src))
                Html.addCodeLine (Html.newPout "")
                Html.addCodeLine (Html.newPerror "")
                Html.addCodeLine (Html.newCodeBox "")
        results <- if results.Length > 0 then results.[0 .. results.Length - 2] else results
        results <- sprintf "var results = [%s];" results
        Html.addCodeLineTop (Html.newScript results)
        Html.writeToSim ()
        1
