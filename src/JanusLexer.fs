module JanusLexer
open System.Collections.Generic
open System.Text.RegularExpressions
let mutable getToken_i = 0
let lex inStr =
    let linesIndex = new List<int>()
    let linesIndexLast () =
        if linesIndex.Count = 0 then 0
        else linesIndex.[linesIndex.Count - 1]
    let tokens = new List<JanusParser.token>()
    let regToken =
        // Comments
        "//[^\\n]*|"+
        // Tokens
        "([a-zA-Z][a-zA-Z_0-9]*)|"+
        "([0-9]+)|"+
        "(\\[|\\]|\\(|\\))|"+
        "(-=|\\+=|\\^=|,|\\^|\\+|-|\\*|\\/|%|&&|&|\\|\\||\\||<=>)|"+
        "(<=|>=|<|>|!=|=)|"+
        // Ignore
        " +|\\n"
    let addToken xIndex (tGroup : GroupCollection) =
        let pos = (linesIndex.Count + 1,xIndex - (linesIndexLast()),xIndex)
        if tGroup.[1].Value <> "" then
            match tGroup.[1].Value with
            | "int"         -> tokens.Add(JanusParser.token.INT pos)
            | "stack"       -> tokens.Add(JanusParser.token.STACK pos)
            | "main"        -> tokens.Add(JanusParser.token.MAIN pos)
            | "procedure"   -> tokens.Add(JanusParser.token.PROC pos)
            | "if"          -> tokens.Add(JanusParser.token.IF pos)
            | "then"        -> tokens.Add(JanusParser.token.THEN pos)
            | "else"        -> tokens.Add(JanusParser.token.ELSE pos)
            | "fi"          -> tokens.Add(JanusParser.token.FI pos)
            | "from"        -> tokens.Add(JanusParser.token.FROM pos)
            | "do"          -> tokens.Add(JanusParser.token.DO pos)
            | "loop"        -> tokens.Add(JanusParser.token.LOOP pos)
            | "until"       -> tokens.Add(JanusParser.token.UNTIL pos)
            | "push"        -> tokens.Add(JanusParser.token.PUSH pos)
            | "pop"         -> tokens.Add(JanusParser.token.POP pos)
            | "local"       -> tokens.Add(JanusParser.token.LOCAL pos)
            | "delocal"     -> tokens.Add(JanusParser.token.DELOCAL pos)
            | "call"        -> tokens.Add(JanusParser.token.CALL pos)
            | "uncall"      -> tokens.Add(JanusParser.token.UNCALL pos)
            | "empty"       -> tokens.Add(JanusParser.token.EMPTY pos)
            | "top"         -> tokens.Add(JanusParser.token.TOP pos)
            | "skip"        -> tokens.Add(JanusParser.token.SKIP pos)
            | "show"        -> tokens.Add(JanusParser.token.SHOW pos)
            | "size"        -> tokens.Add(JanusParser.token.SIZE pos)
            | id            -> tokens.Add(JanusParser.token.VAL_ID (id,pos))
        if tGroup.[2].Value <> "" then
            tokens.Add(JanusParser.token.VAL_INT (tGroup.[2].Value,pos))
        if tGroup.[3].Value <> "" then
            match tGroup.[3].Value with
            | "["   -> tokens.Add(JanusParser.token.LBRACKET pos)
            | "]"   -> tokens.Add(JanusParser.token.RBRACKET pos)
            | "("   -> tokens.Add(JanusParser.token.LPAR pos)
            | ")"   -> tokens.Add(JanusParser.token.RPAR pos)
            | _     -> ()
        if tGroup.[4].Value <> "" then
            match tGroup.[4].Value with
            | "-="  -> tokens.Add(JanusParser.token.BIND_MINUS pos)
            | "+="  -> tokens.Add(JanusParser.token.BIND_PLUS pos)
            | "^="  -> tokens.Add(JanusParser.token.BIND_UP pos)
            | ","   -> tokens.Add(JanusParser.token.COMMA pos)
            | "+"   -> tokens.Add(JanusParser.token.PLUS pos)
            | "-"   -> tokens.Add(JanusParser.token.MINUS pos)
            | "*"   -> tokens.Add(JanusParser.token.TIMES pos)
            | "^"   -> tokens.Add(JanusParser.token.UP pos)
            | "%"   -> tokens.Add(JanusParser.token.PERC pos)
            | "&"   -> tokens.Add(JanusParser.token.AMP pos)
            | "&&"  -> tokens.Add(JanusParser.token.AMPAMP pos)
            | "|"   -> tokens.Add(JanusParser.token.MID pos)
            | "||"  -> tokens.Add(JanusParser.token.MIDMID pos)
            | "<=>" -> tokens.Add(JanusParser.token.IFF pos)
            | "/"   -> tokens.Add(JanusParser.token.DIV pos)
            | v0    -> failwith (sprintf "not defined token '%s'" v0) 
        if tGroup.[5].Value <> "" then
            match tGroup.[5].Value with
            | "<="  -> tokens.Add(JanusParser.token.LEQ pos)
            | "<"   -> tokens.Add(JanusParser.token.LT pos)
            | ">="  -> tokens.Add(JanusParser.token.GEQ pos)
            | ">"   -> tokens.Add(JanusParser.token.GT pos)
            | "="   -> tokens.Add(JanusParser.token.EQ pos)
            | "!="  -> tokens.Add(JanusParser.token.NEQ pos)
            | _     -> ()
    let matchF (m : Match) =
        if m.Value = "\n" then linesIndex.Add(m.Index)
        else addToken m.Index m.Groups
        ""
    let residueStr = Regex.Replace(inStr,regToken,matchF)
    if residueStr <> "" then
        failwith ("garbage in program: '"+residueStr+"'")
    else
        tokens.Add(JanusParser.token.EOI (linesIndex.Count + 1,linesIndexLast(),inStr.Length - 1))
        Array.init
            (tokens.Count)
            (fun _ ->
                let t = tokens.[0]
                tokens.RemoveAt(0)
                t
                )
let getNextToken (tokens : JanusParser.token []) i =
    if tokens.Length = 0 || getToken_i >= tokens.Length then
        failwith "trying to get token from empty buffer"
    else
        let t = tokens.[getToken_i]
        getToken_i <- getToken_i + 1
        t
let getPrevTokenVal (tokens : JanusParser.token []) =
    if tokens.Length = 0 || getToken_i > tokens.Length then
        failwith "out of bound token getting"
    elif getToken_i = 0 then
        tokens.[0]
    else
        tokens.[getToken_i - 1]
let clearModule () =
    getToken_i <- 0
