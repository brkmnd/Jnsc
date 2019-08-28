module Janus2Bob
open System.Collections.Generic
open JanusAbSyn

(* Instruction Args
 * *)
type InstrArg =
    | Address of InstrArg
    | Imm of string
    | Register of string
    | Id of string
    | BinExpr of string * InstrArg * InstrArg
    | Compound of string * InstrArg list
    | Null
    with
    static member (+) (arg1,arg2) = BinExpr ("+",arg1,arg2)
    static member (-) (arg1,arg2) = BinExpr ("-",arg1,arg2)
    static member (*) (arg1,arg2) = BinExpr ("*",arg1,arg2)
    static member fromArg(arg) =
        match arg with
        | Atom.Id (_,_,id,_) -> Register id
        | Val v -> Imm v
        | _ -> failwith "not yet in InstrArg.fromArg"
    static member fromArgs (args) = List.map InstrArg.fromArg args
    static member fromExpr (e) =
        match e with
        | Literal l -> InstrArg.fromArg l
        | _ -> failwith "not yet in InstrArg.fromExpr"
    static member toString(arg) =
        match arg with
        | Compound (_,args) ->
            let sargs =
                List.fold
                    (fun acc x ->
                        acc + InstrArg.toString (x) + ","
                        )
                    ""
                    args
            if sargs.Length > 0 then "(" + sargs.Substring(0,sargs.Length - 1) + ")"
            else "()"
        | Address addr -> "[" + InstrArg.toString(addr) + "]"
        | Imm v -> v
        | Register r -> sprintf "$%s" r
        | Id id -> id
        | BinExpr (op,arg1,arg2) ->
            (InstrArg.toString arg1) +
            " " +
            op +
            " " +
            (InstrArg.toString arg2)
        | _ -> "null"
    static member newCallArgs (args) =
        Compound ("call-args",InstrArg.fromArgs args)
    static member newCallArgs (args) =
        Compound ("call-args",args)
    static member newLabel (id) = Id id
    static member isNull (arg) =
        match arg with
        | Null -> true
        | _ -> false
    static member isImmZero (arg) =
        match arg with
        | Imm "0" -> true
        | _ -> false
    static member isImmOne (arg) =
        match arg with
        | Imm "1" -> true
        | _ -> false
    static member isImm (arg) =
        match arg with
        | Imm _ -> true
        | _ -> false
    static member isAddr (arg) =
        match arg with
        | Address _ -> true
        | _ -> false
    static member isReg (arg) = 
        match arg with
        | Register _ -> true
        | _ -> false
    static member isId (arg) =
        match arg with
        | Id _ -> true
        | _ -> false
    static member unfoldAddress (arg) =
        match arg with
        | Address addr -> addr
        | _ -> arg
(* Translate State
 * Contains depth of if-statements, callee and caller regs and
 * so on
 * *)
type TransState (procName) =
    let mutable dIf = 0
    let mutable dMisc = 0
    let mutable dLoop = 0
    let mutable dCallee = 0
    let mutable dLocal = 0
    member this.procName = procName
    member this.depthIfGet () = dIf 
    member this.depthIfGetAdd () =
        let r = dIf
        dIf <- dIf + 1
        r
    member this.depthLoopGetAdd () =
        let l = dLoop
        dLoop <- l + 1
        l
    member this.depthMiscGetAdd () =
        let d = dMisc
        dMisc <- d + 1
        d
    member this.depthMiscGet () = dMisc
    member this.depthLocalAdd() = dLocal <- dLocal + 1
    member this.depthLocalGetAdd () =
        let d = dLocal
        this.depthLocalAdd()
        d
    member this.calleeGet () =
        Register (sprintf "%s.%d" procName dCallee)
    member this.calleeGetAdd () =
        let c = dCallee
        dCallee <- dCallee + 1
        Register (sprintf "%s.%d" procName c)
    member this.calleeDepthGet () = dCallee
    member this.calleeDepthSet (v) = dCallee <- v
    member this.local (name) =
        // Add depth in order to accomondate for
        // same local var being used again
        Register (sprintf "%s.%s.%d" procName name dLocal)
    member this.rAddr = Register "raddr"
    member this.sp = Register "spointer"
(* Global state - warnings and so on *)
let warnings = new List<string>()
let addWarning (w) =
    warnings.Add(sprintf "warning: %s" w)
let clearGs () =
    warnings.Clear()
(* Module holding BobCode - some funs more abstract than the BOB ISA
 * Every function in here should return a string containing BOB code
 * *)            
module BobCode = 
    let ind1 = "    "
    let ind2 = ind1 + ind1
    let rec removeTrailingBr s =
        let len = String.length s
        if len > 0 && s.[len - 1] = '\n' then
            removeTrailingBr s.[0 .. len - 2]
        else s
    let entry =
        "bob.prg.entry:\n"+
        (sprintf "%s%-12s%s\n" ind1 "CALL" "main ()")+
        (sprintf "%sSTOP\n" ind1)
    let new0Arg instr () =
        sprintf "%s%-12s\n" ind1 instr
    let new1Arg instr arg1 =
        sprintf "%s%-12s%s\n" ind1 instr (InstrArg.toString arg1)
    let new2Arg n (arg1,arg2) =
        let sarg1 = InstrArg.toString arg1
        let sarg2 = InstrArg.toString arg2
        sprintf "%s%-12s%s  %s\n" ind1 n sarg1 sarg2
    let new3Arg n (arg1,arg2,arg3) =
        let sarg1 = InstrArg.toString arg1
        let sarg2 = InstrArg.toString arg2
        let sarg3 = InstrArg.toString arg3
        sprintf "%s%-12s%s  %s  %s\n" ind1 n sarg1 sarg2 sarg3
    // Alter BobCode
    let invertCode c =
        let len = String.length c
        if len = 0 then c
        else
            let c0 =
                if c.[len - 1] = '\n' then c.[0 .. len - 2]
                else c
            let a = c0.Split ('\n')
            System.String.Join ("\n",Array.rev a) + "\n"
    let removeFirstInstr c =
        let len = String.length c
        let i = c.IndexOf('\n')
        if i > -1 && i < len - 1 then c.[i + 1 ..]
        elif i > -1 then ""
        else c
    let removeLastInstr c =
        let c0 = removeTrailingBr c
        let len = String.length c0
        let i = c0.LastIndexOf('\n')
        if i > 0 && i < len - 1 then c0.[0 .. i]
        else c
    // Comments
    let comm c = sprintf "%s# %s\n" ind1 c
    let commFunBody id = comm (sprintf "%s body" id)
    let commMainDecs = comm "main declarations"
    let commMainUndecs = comm "main undeclarations"
    let splitter = "<--split-->"
    // Labels
    let label = function
        | Id id -> sprintf "%s:\n" id
        | _ -> failwith "not id in BobCode.label"
    // Instrs - arit
    let add = new2Arg "ADD"
    let add1 = new1Arg "ADD1"
    let sub = new2Arg "SUB"
    let sub1 = new1Arg "SUB1"
    let swap = new2Arg "SWAP"
    let xor = new2Arg "XOR"
    let xori = new2Arg "XORI"
    let neg = new1Arg "NEG"
    let mul = new3Arg "MUL"
    let mul2 = new1Arg "MUL2"
    let div = new3Arg "DIV"
    let div2 = new3Arg "DIV2"
    let modop = new3Arg "MOD"
    // Instr - Logical
    let band = new3Arg "AND"
    let bor  = new3Arg "OR"
    // Instrs - branch
    let call = new2Arg "CALL"
    let rcall = new2Arg "RCALL"
    let swret = new1Arg "SWRET"
    let bra = new1Arg "BRA"
    let swbr = new1Arg "SWBR"
    let beq = new3Arg "BEQ"
    let blt = new3Arg "BLT"
    let bgt = new3Arg "BGT"
    let bleq = new3Arg "BLEQ"
    let bgeq = new3Arg "BGEQ"
    let bneq = new3Arg "BNEQ"
    let bnz = new2Arg "BNZ"
    let bz = new2Arg "BZ"
    // Instrs - stack
    let push = new1Arg "PUSH"
    let pop = new1Arg "POP"
    // Instrs - mem
    let exch = function
        | (arg1,Address arg2) -> new2Arg "EXCH" (arg1,arg2)
        | (arg1,arg2) -> new2Arg "EXCH" (arg1,arg2)
    let nop = new0Arg "NOP"
    let echo = new1Arg "ECHO"
    // Code builders
    let range f i =
        let rec exec j acc0 =
            if i <= j then acc0
            else exec (j + 1) (f j acc0)
        exec 0

(* Scope as class that is instantiated
 * The core scope is a map of string |-> InstrArg
 * *)
type Scope (args : Atom list) =
    let add2scope i (scope : Dictionary<string,InstrArg>) = function
        | Atom.Id (_,_,id,_) ->
            let v = Address (Register (sprintf "arg%d" i))
            scope.Add(id,v)
            (i+1,scope)
        | _ -> (i,scope)
    let (_,scope) =
        List.fold
            (fun (i,sc : Dictionary<string,InstrArg>) arg -> add2scope i sc arg)
            (0,new Dictionary<string,InstrArg>())
            args
    let fetched =
        // A dictionary containing pointer that have been
        // fetched with exch. If these are exch'ed again
        // the program will have undefined behavior
        new Dictionary<InstrArg,InstrArg>()
    member this.get(id : string) =
        if scope.ContainsKey (id) then scope.[id]
        else failwith (sprintf "'%s' not in scope" id)
    member this.get(id : Atom) =
        match id with
        | Atom.Id (_,_,id,_) -> this.get (id)
        | _ -> failwith (sprintf "'%A' can't be used to access scope" id)
    member this.getIgn(id : Atom) =
        match id with
        | Atom.Id (_,_,id,_) ->
            if scope.ContainsKey(id) then scope.[id]
            else Register id
        | Val v -> Imm v
        | _ -> failwith "not yet scope.getIgn"
    member this.getIgn(id : string) =
        if scope.ContainsKey (id) then scope.[id]
        else Null
    member this.getIgn (a : InstrArg) =
        match a with
        | Register r -> if scope.ContainsKey(r) then scope.[r] else a
        | Id i -> if scope.ContainsKey(i) then scope.[i] else a
        | _ -> a
    member this.contains (key) = scope.ContainsKey(key)
    member this.add(id,v) =
        if scope.ContainsKey(id) then scope.[id] <- v
        else scope.Add(id,v)
    member this.remove(id) =
        if scope.ContainsKey(id) then scope.[id] <- Null
        else failwith (sprintf "'%s' not in scope" id)
    member this.foreach (f) =
        for v in scope do (f v.Key v.Value)
    // fetched methods
    member this.addFetched (k,v) =
        if fetched.ContainsKey(k) then fetched.[k] <- v
        else fetched.Add(k,v)
    member this.getFetched (k) =
        if fetched.ContainsKey (k) then fetched.[k]
        else failwith (sprintf "'%A' not in fetched for get" k)
    member this.containsFetched (k) = fetched.ContainsKey(k)
    member this.clearFetched () = fetched.Clear()
(* Misc Functoins *) 
let isBinOp = function
    | "plus"
    | "minus" -> true
    | _ -> false
let isBinComp = function
    | "lt"
    | "leq"
    | "gt"
    | "geq"
    | "eq"
    | "neq" -> true
    | _ -> false
let invBinCond = function
    | "eq" -> "neq"
    | "neq" -> "eq"
    | op -> failwith (sprintf "not bin cond '%s'" op) 
(* Traverse starting point
 * Traversing returns a string of code on success
 * *)
let rec traverse = function
    | Procs prcs ->
        BobCode.entry + (traverseProcs "" prcs)
    | Error -> "error"
and traverseProcs acc = function
    | (Proc (name,args,body))::prcs ->
        let scope =
            if name = "main" then new Scope ([])
            elif List.length args > 4 then new Scope (args.[0 .. 3])
            else new Scope(args)
        let state = new TransState (name)
        let l_top =
            BobCode.label (Id (sprintf "%s.top" name)) +
            BobCode.bra (Id (sprintf "%s.bot" name)) +
            BobCode.pop state.rAddr
        let l_proc =
            BobCode.label (Id name) +
            BobCode.swret state.rAddr +
            BobCode.neg state.rAddr +
            BobCode.push state.rAddr
        let code_proc =
            BobCode.commFunBody name +
            traverseStmts "" scope state body
        let l_bot =
            BobCode.label (Id (sprintf "%s.bot" name)) +
            BobCode.bra (Id (sprintf "%s.top" name))
        let acc0 = acc + l_top + l_proc + code_proc + l_bot
        traverseProcs acc0 prcs
    | x::prcs -> traverseProcs acc prcs
    | [] -> acc
and traverseStmts acc (scope : Scope) state stmts0 =
    match stmts0 with
    | (Decs decs)::stmts ->
        let pre_decs,post_decs = traverseDecs scope state decs
        let code_stmts = traverseStmts acc scope state stmts
        let code =
            BobCode.commMainDecs +
            pre_decs +
            code_stmts +
            BobCode.commMainUndecs +
            post_decs
        acc + code
    | (Call (id,args,_))::stmts
    | (Uncall (id,args,_))::stmts ->
        let f arg =
            // Typechecker checks whether in scope
            // Unfold since we pass as value in BOB
            let sarg = Atom.toString arg
            InstrArg.unfoldAddress (scope.get (sarg))
        let pushExtraF acc arg =
            let sarg = Atom.toString arg
            let id_arg = sprintf "%s.%s" id sarg
            let reg_ptr = InstrArg.unfoldAddress (scope.get (sarg))
            let reg_id = Register id_arg
            acc +
            // save old id in order to zero-clear register for new
            BobCode.push (reg_id) +
            BobCode.xor (reg_id,reg_ptr)
        let popExtraF acc arg =
            let sarg = Atom.toString arg
            let id_arg = sprintf "%s.%s" id sarg
            let reg_ptr = InstrArg.unfoldAddress (scope.get (sarg))
            let reg_id = Register id_arg
            acc +
            BobCode.xor (reg_id,reg_ptr) +
            BobCode.pop (reg_id)
        let callArgs =
            let a = if List.length args > 4 then args.[0 .. 3] else args
            InstrArg.newCallArgs (List.map f a)
        let code_pre,code_post =
            if List.length args > 4 then
                // When more than 4 args is given,
                // we push the rest onto the stack.
                // These are not put into scope, but instead
                // a register with given name is set up
                let a = args.[3 ..];
                let pre = List.fold pushExtraF "" a
                let post = List.fold popExtraF "" a
                (pre,post)
            else ("","")
        let code_call =
            match stmts0.[0] with
            | Uncall _ -> BobCode.rcall (Id id,callArgs)
            | _ -> BobCode.call (Id id,callArgs)
        let code =
            code_pre +
            code_call +
            code_post
        traverseStmts (acc + code) scope state stmts
    | (BindOp (op,atom,expr,pos))::stmts ->
        // On bind-ops the op is reversed during uncall
        // When exhcange of left-hand ptr is done, 0 is placed at that mem address
        // ensuring that if left id is present on right side, undef beh.
        let comm_name = Stmt.toString (BindOp (op,atom,expr,pos))
        let comm_top = BobCode.comm comm_name
        let comm_bot = BobCode.comm (sprintf "/%s" comm_name)
        let code_pre_ptr,code_post_ptr,reg_res = traverseBindPtr scope state atom
        let code_pre_bind,code_post_bind =
            if expr|>Expr.isLitOne then
                // In this case either add1 or sub1. Rest is caught by Typechecker
                // Needed clean up is done by exch on reg_res
                match op with
                | "plus"    -> (BobCode.add1 (reg_res),"")
                | _         -> (BobCode.sub1 (reg_res),"")
            elif op = "switch" then
                // Typechecker catches if right side is not Atom.Id
                // Clean-up here is to exchange right side back in mem
                let reg_ptr =
                    let id = Expr.toString expr
                    if not (scope.contains (id)) then
                        Register (sprintf "%s.%s" state.procName id)
                    else
                        scope.get id
                let reg_temp = state.calleeGetAdd()
                let pre =
                    BobCode.exch (reg_temp,reg_ptr) +
                    BobCode.swap (reg_res,reg_temp)
                let post =
                    BobCode.exch (reg_temp,reg_ptr)
                (pre,post)
            else
                let c_pre_expr,c_post_expr,reg_expr : (string * string * InstrArg) =
                    traverseExpr scope state expr
                let c_pre_op,c_post_op,reg_op =
                    traverseBindOp scope state op reg_res reg_expr
                (c_pre_expr + c_pre_op,c_post_op + c_post_expr)
        let code =
            comm_top +
            code_pre_ptr +
            code_pre_bind +
            code_post_bind +
            code_post_ptr +
            comm_bot
        traverseStmts (acc + code) scope state stmts
    | (If (expr1,stmts1,stmts2,expr2,_))::stmts ->
        let d_if = state.depthIfGetAdd()
        let l_name =
            sprintf "%s.if%d" state.procName d_if
        let id_top = Id (sprintf "%s_top" l_name)
        let id_else = Id (sprintf "%s_else" l_name)
        let id_if = Id (sprintf "%s_if" l_name)
        let id_then = Id (sprintf "%s_then" l_name) 
        let id_else = Id (sprintf "%s_else" l_name)
        let id_bot = Id (sprintf "%s_bot" l_name)
        let comm_top = BobCode.comm (sprintf "if[%d]" d_if)
        let comm_expr1 = BobCode.comm (sprintf "if[%d](%s)" d_if (Expr.toString expr1))
        let comm_expr2 = BobCode.comm (sprintf "fi[%d](%s)" d_if (Expr.toString expr2))
        let comm_bot = BobCode.comm (sprintf "/if[%d]" d_if)
        // Set up branch arguments when expression
        // ExprDuplicates are used to cover for alternative in/outs
        // hot fix to keep the registers the same
        let calleeD_top = state.calleeDepthGet ()
        let expr_top_pre1,expr_top_post1 =
            // invert condition to match the flow chart
            // that is target is else-branch
            traverseExprBranch scope state true expr1 id_else
        let expr_top_pre2,expr_top_post2 =
            state.calleeDepthSet (calleeD_top)
            traverseExprBranch scope state true expr1 id_else
        let calleeD_bot = state.calleeDepthGet ()
        let expr_bot_pre1,expr_bot_post1 =
            // do not invert condition, target is then-branch
            traverseExprBranch scope state false expr2 id_then
        let expr_bot_pre2,expr_bot_post2 =
            state.calleeDepthSet (calleeD_bot)
            traverseExprBranch scope state false expr2 id_then
        let l_top =
            expr_top_pre1 +
            BobCode.label id_top +
            expr_top_post1
        let l_if =
            BobCode.label id_if +
            traverseStmts "" scope state stmts1
        let l_then =
            expr_bot_pre2 +
            BobCode.label id_then +
            BobCode.bra id_bot
        let l_else =
            expr_top_pre2 +
            BobCode.label id_else +
            BobCode.bra id_top +
            // first instr is bz from if check
            (BobCode.removeFirstInstr expr_top_post2) +
            traverseStmts "" scope state stmts2
        let l_bot =
            expr_bot_pre1 +
            BobCode.label id_bot +
            expr_bot_post1
        let code =
            comm_top +
            comm_expr1 +
            l_top +
            l_if +
            l_then +
            l_else +
            comm_expr2 +
            l_bot +
            comm_bot
        traverseStmts (acc + code) scope state stmts
    | (Local (id1,e1,s,id2,e2,pos))::stmts ->
        // Local do not yet handle type.
        // That is arrays can't be created as local variables.
        let name_var_pre = (Atom.toString id1)
        let name_var_post = (Atom.toString id2)
        let name_local = sprintf "local[%s = %s]%A" name_var_pre (Expr.toString e1) (fst pos)
        let name_delocal = sprintf "delocal[%s = %s]%A" name_var_post (Expr.toString e2) (snd pos)
        let reg_local_pre,reg_local_post =
            // These two regs should match in name, else undef. beh.
            (state.local name_var_pre,state.local name_var_post)
        let reg_temp =
            // Store result for push/pop
            // push/pop inserts a zero, this reg is used to
            // avoid clean up
            state.calleeGetAdd()
        let code_pre_e1,code_post_e1,reg_e1 = traverseExpr scope state e1
        let code_pre_e2,code_post_e2,reg_e2 = traverseExpr scope state e2
        let code_local =
            // use reg_temp to hold res to avoid clean up after push
            BobCode.comm name_local +
            code_pre_e1 +
            BobCode.xor (reg_temp,reg_e1) +
            BobCode.push (reg_temp) +
            BobCode.xor (reg_local_pre,state.sp) +
            code_post_e1 +
            BobCode.comm (sprintf "/%s" name_local)
        let code_delocal =
            // use reg_temp to hold res to avoid clean up after pop
            BobCode.comm name_delocal +
            code_pre_e2 +
            BobCode.xor (reg_local_post,state.sp) +
            BobCode.pop (reg_temp) +
            BobCode.xor (reg_temp,reg_e2) +
            code_post_e2 +
            BobCode.comm (sprintf "/%s" name_delocal)
        let code_stmts =
            let id_scope = name_var_pre
            let save_scopeval =
                // With string ign returns Null when not present
                scope.getIgn (id_scope)
            let sadd = scope.add(id_scope,Address reg_local_pre)
            let res = traverseStmts "" scope state s
            let sinsert =
                // Typechecker catches if var is used outside of scope
                // thus Null insert is not a problem
                scope.add(id_scope,save_scopeval)
            res
        let code = code_local + code_stmts + code_delocal
        let add_depth = state.depthLocalAdd()
        traverseStmts (acc + code) scope state stmts
    | (From (expr1,stmt1,stmt2,expr2,pos))::stmts ->
        // Conditional with same structure as If
        // that is clean up conditions both if branch is
        // taken and if not
        let l_name =
            sprintf "%s.loop%d" state.procName (state.depthLoopGetAdd())
        let comm_from = sprintf "from %s at %A" (Expr.toString expr1) (fst pos)
        let comm_until = sprintf "until %s at %A" (Expr.toString expr2) (snd pos)
        let id_top = Id (sprintf "%s_top" l_name)
        let id_do = Id (sprintf "%s_do" l_name)
        let id_loop = Id (sprintf "%s_loop" l_name)
        let id_bot = Id (sprintf "%s_bot" l_name)
        let calleeD_top = state.calleeDepthGet ()
        let expr_top_pre1,expr_top_post1 =
            traverseExprBranch scope state true expr1 id_loop
        let expr_top_pre2,expr_top_post2 =
            state.calleeDepthSet (calleeD_top)
            traverseExprBranch scope state true expr1 id_loop
        let calleeD_bot = state.calleeDepthGet ()
        let expr_bot_pre1,expr_bot_post1 =
            traverseExprBranch scope state false expr2 id_bot
        let expr_bot_pre2,expr_bot_post2 =
            state.calleeDepthSet (calleeD_bot)
            traverseExprBranch scope state false expr2 id_bot
        let code_do =
            traverseStmts "" scope state stmt1
        let code_loop =
            traverseStmts "" scope state stmt2
        let code =
            BobCode.comm comm_from +
            expr_top_pre1 +
            BobCode.label id_top +
            expr_top_post1 +
            BobCode.comm "do-code" +
            code_do +
            BobCode.comm comm_until +
            expr_bot_pre1 +
            BobCode.label id_do +
            expr_bot_post1 +
            code_loop +
            expr_top_pre2 +
            BobCode.label id_loop +
            BobCode.bra id_top +
            (BobCode.removeFirstInstr expr_top_post2) +
            expr_bot_pre2 +
            BobCode.label id_bot +
            BobCode.bra id_do +
            (BobCode.removeFirstInstr expr_bot_post2)
        traverseStmts (acc + code) scope state stmts
    | Skip::stmts ->
        let code =
            BobCode.comm "skip" +
            BobCode.nop()
        traverseStmts (acc + code) scope state stmts
    | s::stmts ->
        addWarning (sprintf "not yet in traverseStmts %A" s)
        traverseStmts acc scope state stmts
    | [] -> acc
and traverseBindPtr (scope : Scope) (state : TransState) = function
    // Typehecker ensures that atom is of id type and is in scope
    // Program will go bad if target id is in right side expr
    // Returns pre-code * post-code * reg with result to be exchanged back
    | Index (aptr,ind,_) ->
        let id = Atom.toString aptr
        let reg_ptr_arg =
            // If not in scope, then the array is an extra arg
            // In this case we ignore and use $proc.id as ptr
            if not (scope.contains (id)) then
                Register (sprintf "%s.%s" state.procName id)
            else
                InstrArg.unfoldAddress (scope.get id)
        let reg_ptr = state.calleeGetAdd()
        let reg_target = state.calleeGetAdd()
        let pre_expr,post_expr,reg_expr = traverseExpr scope state ind
        let pre =
            pre_expr +
            BobCode.xor (reg_ptr,reg_ptr_arg) +
            BobCode.add1 (reg_ptr) +
            BobCode.add (reg_ptr,reg_expr) +
            BobCode.exch (reg_target,reg_ptr)
        let post =
            BobCode.exch (reg_target,reg_ptr) +
            BobCode.sub (reg_ptr,reg_expr) +
            BobCode.sub1 (reg_ptr) +
            BobCode.xor (reg_ptr,reg_ptr_arg) +
            post_expr
        (pre,post,reg_target)
    | atom ->
        let id =
            // We don't worry about type of atom is not Id
            Atom.toString atom
        let reg = state.calleeGetAdd()
        let reg_ptr =
            if not (scope.contains (id)) then
                // If not in scope, then atom is in extra arg
                // In this case we ignore and use $id as ptr
                let reg = Register (sprintf "%s.%s" state.procName id)
                Address reg
            else
                scope.get (id)
        let pre = BobCode.exch (reg,reg_ptr)
        let post = pre
        (pre,post,reg)
and traverseDecs (scope : Scope) (state : TransState) = function
    // Assume dir = 0, that is main cannot be uncalled
    | (Atom.Id (Array _,atts,id,_))::decs ->
        let imm_size = Imm (string atts.size)
        let reg_size = state.calleeGetAdd()
        let f i (pre,post) =
            let elmId = sprintf "%s@%d" id i
            let code_alloc =
                BobCode.push (Register "0") +
                BobCode.xor (Register elmId,state.sp)
            let code_dealloc =
                // use fresh regs
                BobCode.pop (Register (sprintf "res.%s" elmId))
            (pre + code_alloc,code_dealloc + post)
        let scope0 =
            scope.add (id,Address (Register id))
            scope
        let code_pre,code_post =
            // Create array
            BobCode.range f atts.size ("","")
        let code_alloc =
            // Push array head with size
            BobCode.xori (reg_size,imm_size) +
            BobCode.push (reg_size) +
            BobCode.xor (Register id,state.sp) +
            code_pre
        let code_dealloc =
            // Let array head stay internal
            code_post +
            BobCode.pop (reg_size) +
            BobCode.xori (reg_size,imm_size)
        let next_pre,next_post = traverseDecs scope0 state decs
        (code_alloc + next_pre,next_post + code_dealloc)
    | (Atom.Id (Int,_,id,_))::decs ->
        let code_alloc =
            BobCode.push (Register "0") +
            BobCode.xor (Register id,state.sp)
        let code_dealloc =
            // use fresh regs in order to zero clear stack
            BobCode.pop (Register (sprintf "res.%s" id))
        let scope0 =
            scope.add (id,Address (Register id))
            scope
        let next_pre,next_post = traverseDecs scope0 state decs
        (code_alloc + next_pre,next_post + code_dealloc)
        //traverseDecs acc0 scope0 state decs
    | [] -> ("","")
    // Syntactically impossible
    | _::decs -> traverseDecs scope state decs
and traverseExpr (scope : Scope) (state : TransState) = function
    // Returns pre-code * post-code * target register (holding result)
    // Shortcut expressions are the kind that can omit EXCH and the like
    // on at least one of the operands
    | BinOp ("minus",Literal l,Literal r,_) when l|>Atom.isVal && r|>Atom.isVal ->
        // Shortcut expression
        let reg_target = state.calleeGetAdd()
        let val_target = InstrArg.fromArg (l - r)
        let pre = BobCode.sub (reg_target,val_target)
        let post = BobCode.add (reg_target,val_target)
        (pre,post,reg_target)
    | BinOp ("minus",l,Literal r,_) when r|>Atom.isVal ->
        // Shortcut expression
        let pre_l,post_l,reg_target = traverseExpr scope state l
        let imm_r = InstrArg.fromArg (r)
        let pre = pre_l + BobCode.sub (reg_target,imm_r)
        let post = BobCode.add (reg_target,imm_r) + post_l
        (pre,post,reg_target)
    | BinOp (op,l,r,_) ->
        // Treated as full expression
        // In post code invert order of instrs when dealing with literals
        let pre_l,post_l,reg_l = traverseExpr scope state l
        let pre_r,post_r,reg_r = traverseExpr scope state r
        let pre_temp,post_temp,reg_temp =
            // Store result here in order to
            // avoid altering mem when
            // exch back
            let reg = state.calleeGetAdd()
            let pre_post = BobCode.xor (reg,reg_l)
            (pre_post,pre_post,reg)
        let pre_op,post_op,reg_op =
            let pre,post,reg = traverseBinOp scope state op reg_temp reg_r
            (pre_temp + pre,post + post_temp,reg)
        let pre = pre_l + pre_r + pre_op
        let post = post_op + post_r + post_l
        (pre,post,reg_op)
    | UnaOp (_,e,_) ->
        // Op can only be minus
        let pre_e,post_e,reg_e = traverseExpr scope state e
        let reg_target = state.calleeGetAdd()
        let pre =
            pre_e +
            BobCode.sub (reg_target,reg_e)
        let post =
            BobCode.add (reg_target,reg_e) +
            post_e
        (pre,post,reg_target)
    | Literal a -> traverseAtom scope state a
and traverseAtom (scope : Scope) (state : TransState) = function
    // Returns a tuple of pre code, post code and target register
    | Atom.Id (_,_,id,_) ->
        // In this case from pointer
        let reg_ptr =
            // If not in scope, then ptr is an extra arg
            // In this case ptr is $id
            if not (scope.contains (id)) then
                let reg = Register (sprintf "%s.%s" state.procName id)
                Address reg
            else
                scope.get (id)
        let reg_temp = state.calleeGetAdd()
        let reg_target = state.calleeGetAdd()
        let code_pre =
            BobCode.exch (reg_temp,reg_ptr) +
            BobCode.xor (reg_target,reg_temp) +
            BobCode.exch (reg_temp,reg_ptr)
        let code_post = code_pre
        (code_pre,code_post,reg_target)
    | Val v ->
        // In this allocate register to hold value 
        // Clean up target reg in post
        let reg_target = state.calleeGetAdd()
        let code = BobCode.xori (reg_target,Imm v)
        (code,code,reg_target)
    | Index (id,ind,pos) ->
        let sid = Atom.toString id
        let name = sprintf "indexing %s[%s]%A" (Atom.toString id) (Expr.toString ind) pos
        let reg_ptr = state.calleeGetAdd()
        let reg_temp = state.calleeGetAdd()
        let reg_target = state.calleeGetAdd()
        let pre_ind,post_ind,reg_ind = traverseExpr scope state ind
        let reg_arg_ptr =
            // If id not in scope, then extra arg
            // In this case use $id as ptr
            if not (scope.contains (sid)) then
                Register (sprintf "%s.%s" state.procName sid)
            else
                InstrArg.unfoldAddress (scope.get sid)
        let pre_ptr =
            BobCode.xor (reg_ptr,reg_arg_ptr) +
            BobCode.add (reg_ptr,reg_ind) +
            BobCode.add1 (reg_ptr) +
            BobCode.exch (reg_temp,reg_ptr) +
            BobCode.xor (reg_target,reg_temp) +
            BobCode.exch (reg_temp,reg_ptr)
        let post_ptr =
            BobCode.exch (reg_temp,reg_ptr) +
            BobCode.xor (reg_target,reg_temp) +
            BobCode.exch (reg_temp,reg_ptr) +
            BobCode.sub1 (reg_ptr) +
            BobCode.sub (reg_ptr,reg_ind) +
            BobCode.xor (reg_ptr,reg_arg_ptr)
        let code_pre =
            BobCode.comm name +
            pre_ind +
            pre_ptr +
            BobCode.comm "ind-expr"
        let code_post =
            BobCode.comm "/ind-expr" +
            post_ptr +
            post_ind +
            BobCode.comm (sprintf "/%s" name)
        (code_pre,code_post,reg_target)
    | CallBuildin ("size",[Atom.Id (_,_,id,_)],_) ->
        // Typechecker checks whether in scope
        let name = sprintf "size(%s)" id
        let reg_size = state.calleeGetAdd()
        let reg_addr = InstrArg.unfoldAddress (scope.get(id))
        let reg_temp = state.calleeGetAdd()
        let pre =
            BobCode.comm name +
            BobCode.exch (reg_temp,reg_addr) +
            BobCode.xor (reg_size,reg_temp) +
            BobCode.exch (reg_temp,reg_addr)
        let post =
            BobCode.exch (reg_temp,reg_addr) +
            BobCode.xor (reg_size,reg_temp) +
            BobCode.exch (reg_temp,reg_addr) +
            BobCode.comm ("/" + name)
        (pre,post,reg_size)
    | _ -> failwith "not yet traverseAtom in Janus2BOB"
and traverseBinOp (scope : Scope) (state : TransState) op reg1 reg2 =
    // Returns pre, post and reg holding result
    // These are symmetrical, that is clean up is done
    let tuple_res2 instr invInstr =
        let pre = instr (reg1,reg2)
        let post = invInstr (reg1,reg2)
        (pre,post,reg1)
    let tuple_res3 instr invInstr =
        let reg_target = state.calleeGetAdd()
        let pre = instr (reg_target,reg1,reg2)
        let post = invInstr (reg_target,reg1,reg2)
        (pre,post,reg_target)
    let exprCompare name op invOp =
        let id_true_pre = Id (sprintf "%s_true_pre" name)
        let id_true_post = Id (sprintf "%s_true_post" name)
        let id_false_pre = Id (sprintf "%s_false_pre" name)
        let id_false_post = Id (sprintf "%s_false_post" name)
        let id_top_pre = Id (sprintf "%s_top_pre" name)
        let id_top_post = Id (sprintf "%s_top_post" name)
        let id_bot_pre = Id (sprintf "%s_bot_pre" name)
        let id_bot_post = Id (sprintf "%s_bot_post" name)
        let reg_target = state.calleeGetAdd()
        let code_pre =
            BobCode.label id_top_pre +
            op (reg1,reg2,id_true_pre) +
            BobCode.label id_false_pre +
            BobCode.bra id_bot_pre +
            BobCode.label id_true_pre +
            BobCode.bra id_top_pre +
            BobCode.xori (reg_target,Imm "1") +
            BobCode.label id_bot_pre +
            invOp (reg1,reg2,id_false_pre) +
            BobCode.comm (sprintf "/%s_pre" name)
        let code_post =
            // Move labels one up since bob lands on
            // first instr right under label
            BobCode.label id_bot_post +
            invOp (reg1,reg2,id_false_post) +
            BobCode.xori (reg_target,Imm "1") +
            BobCode.label id_true_post +
            BobCode.bra id_top_post +
            BobCode.label id_false_post +
            BobCode.bra id_bot_post +
            BobCode.label id_top_post+
            op (reg1,reg2,id_true_post)
        (code_pre,code_post,reg_target)
    match op with
    // arit
    | "plus"    -> tuple_res2 BobCode.add BobCode.sub
    | "minus"   -> tuple_res2 BobCode.sub BobCode.add
    | "up"      -> tuple_res2 BobCode.xor BobCode.xor
    | "times"   -> tuple_res3 BobCode.mul BobCode.mul
    | "div"     -> tuple_res3 BobCode.div BobCode.div
    | "perc"    -> tuple_res3 BobCode.modop BobCode.modop
    // logical
    | "ampamp"  -> tuple_res3 BobCode.band BobCode.band
    | "midmid"  -> tuple_res3 BobCode.bor BobCode.bor
    // compare - return 0 for false and 1 for true
    | "eq"      ->
        let name = sprintf "%s.eq%d" (state.procName) (state.depthMiscGetAdd())
        exprCompare name BobCode.beq BobCode.bneq
    | "neq"     ->
        let name = sprintf "%s.neq%d" (state.procName) (state.depthMiscGetAdd())
        exprCompare name BobCode.bneq BobCode.beq
    | "lt"      ->
        let name = sprintf "%s.lt%d" (state.procName) (state.depthMiscGetAdd())
        exprCompare name BobCode.blt BobCode.bgeq
    | "gt"      ->
        let name = sprintf "%s.gt%d" (state.procName) (state.depthMiscGetAdd())
        exprCompare name BobCode.bgt BobCode.bleq
    | "geq"     ->
        let name = sprintf "%s.geq%d" (state.procName) (state.depthMiscGetAdd())
        exprCompare name BobCode.bgeq BobCode.blt
    | "leq"     ->
        let name = sprintf "%s.leq%d" (state.procName) (state.depthMiscGetAdd())
        exprCompare name BobCode.bleq BobCode.bgt
    // rest
    | _ ->
        let w = sprintf "not imp op '%s' in traverseBinOp" op
        warnings.Add(w)
        (BobCode.comm w,BobCode.nop(),reg1)
and traverseBindOp (scope : Scope) (state : TransState) op reg1 reg2 =
    // Returns pre, post and reg holding result
    // These are not symmetrical.
    // Clean up is done with exch, not here
    let unfoldReg = function
        | Address a ->
            let reg_temp = state.calleeGetAdd()
            let pre = BobCode.exch (reg_temp,a)
            let post = BobCode.exch (reg_temp,a)
            (pre,post,reg_temp)
        | reg -> ("","",reg)
    let tuple_res2 instr =
        let pre1,post1,reg_target1 = unfoldReg reg1
        let pre2,post2,reg_target2 = unfoldReg reg2
        (pre1 + pre2 + instr(reg_target1,reg_target2),post1 + post2,reg_target1)
    match op with
    | "plus"    -> tuple_res2 BobCode.add
    | "minus"   -> tuple_res2 BobCode.sub
    | "up"      -> tuple_res2 BobCode.xor
    // switch is handled in traverseStmts
    | _ ->
        let w = sprintf "'%s' used as BindOp - not reversible" op
        let comm = BobCode.comm (w)
        let code = comm + BobCode.nop()
        addWarning w
        (code,"",reg1)
and traverseExprBranch (scope : Scope) (state : TransState) inv = function
    // Returns lambda from label -> (pre-code,post-code)
    // post here is clean-up code for regs and mem
    (* These are skipped for now
    | BinOp (op,Literal l,Literal r,_) when op|>isBinComp ->
        // We are inside the branch, hence
        // we can handle the branch without any
        // further calculations.
        let arg1 = traverseAtomBranch scope state l
        let arg2 = traverseAtomBranch scope state r
        traverseCompOp scope state inv op [arg1;arg2]
    *)
    | BinOp (op,arg1,arg2,_) ->
        // Here we are outside the branch, that is the condition
        // is compound, ex.
        // - if a + 2 < 0
        // Clean up is needed here
        // Type checker catches expressions that do not return
        // bool
        let name = sprintf "%s[%d]" op (state.depthMiscGet())
        let comm_top = BobCode.comm name
        let comm_bot = BobCode.comm (sprintf "/%s" name)
        let pre_arg1,post_arg1,reg_arg1 = traverseExpr scope state arg1
        let pre_arg2,post_arg2,reg_arg2 = traverseExpr scope state arg2
        let fun_res label =
            let code_res_pre,code_res_post,reg_res = traverseBinOp scope state op reg_arg1 reg_arg2
            let code_if =
                if inv then BobCode.bz (reg_res,label)
                else BobCode.bnz (reg_res,label)
            let code_pre =
                comm_top +
                pre_arg1 + pre_arg2 +
                code_res_pre
            let code_post = code_if + code_res_post + post_arg2 + post_arg1 + comm_bot
            (code_pre,code_post)
        fun_res
    | expr ->
        addWarning (sprintf "not in traverseExprBranch %A" expr)
        fun _ -> (BobCode.nop(),"")
and traverseAtomBranch (scope : Scope) (state : TransState) = function
    // Returns an InstrArg argument. Branches differs since they can
    // handle mem refs. directly
    | Atom.Id (_,_,id,_) ->
        // If not in scope, then extra arg
        // Use $id as ptr
        if not (scope.contains (id)) then
            let reg = Register (sprintf "%s.%s" state.procName id)
            Address reg
        else scope.getIgn(Register id)
    | Val v -> Imm v
    | Index (id,ind,_) ->
        Address (
            // Need to decide on nested ind
            // Add 1 to pass array head
            (InstrArg.unfoldAddress (traverseAtomBranch scope state id)) +
            Imm "1" +
            traverseAtomBranchExpr scope state ind
            )
    | a ->
        addWarning (sprintf "not yet in traverseAtomBranch %A" a)
        Null
and traverseAtomBranchExpr (scope : Scope) (state : TransState) = function
    // Returns an InstrArg expression
    // Within mem [] arit. expressions are accepted by BOB
    // + * and - are overloaded directly in InstrArg
    | BinOp ("plus",arg1,arg2,_) ->
        (traverseAtomBranchExpr scope state arg1) +
        (traverseAtomBranchExpr scope state arg2)
    | BinOp ("minus",arg1,arg2,_) ->
        (traverseAtomBranchExpr scope state arg1) -
        (traverseAtomBranchExpr scope state arg2)
    | BinOp ("times",arg1,arg2,_) ->
        (traverseAtomBranchExpr scope state arg1) *
        (traverseAtomBranchExpr scope state arg2)
    | Literal a ->
        // Might change - mem refs can be nested, ex. [[$reg1] + $reg2]
        InstrArg.unfoldAddress (traverseAtomBranch scope state a)
    | e ->
        addWarning (sprintf "not yet in taverseAtomBranchExpr %A" e)
        Null
and traverseCompOp (scope : Scope) (state : TransState) inv op args =
    // Returns a lambda: label -> pre * post
    // These are conditions that BOB can handle directly.
    // That is no additional code is needed.
    // The branch instruction goes into post code, and pre
    // are left empty
    let isZero = InstrArg.isImmZero
    match (op,args) with
    | ("eq",[arg1;arg2]) ->
        if arg1|>isZero && inv then fun label -> ("",BobCode.bnz (arg2,label))
        elif arg2|>isZero && inv then fun label -> ("",BobCode.bnz (arg1,label))
        elif arg1|>isZero then fun label -> ("",BobCode.bz (arg2,label))
        elif arg2|>isZero then fun label -> ("",BobCode.bz (arg1,label))
        elif inv then fun label -> ("",BobCode.bneq (arg1,arg2,label))
        else fun label -> ("",BobCode.beq (arg1,arg2,label))
    | ("neq",[arg1;arg2]) ->
        if arg1|>isZero && inv then fun label -> ("",BobCode.bz (arg2,label))
        elif arg2|>isZero && inv then fun label -> ("",BobCode.bnz (arg1,label))
        elif arg1|>isZero then fun label -> ("",BobCode.bnz (arg2,label))
        elif arg2|>isZero then fun label -> ("",BobCode.bnz (arg1,label))
        elif inv then fun label -> ("",BobCode.beq (arg1,arg2,label))
        else fun label -> ("",BobCode.bneq (arg1,arg2,label))
    | ("lt",[arg1;arg2]) ->
        if inv then fun label -> ("",BobCode.bgeq (arg1,arg2,label))
        else fun label -> ("",BobCode.blt (arg1,arg2,label))
    | ("leq",[arg1;arg2]) ->
        if inv then fun label -> ("",BobCode.bgt (arg1,arg2,label))
        else fun label -> ("",BobCode.bleq (arg1,arg2,label))
    | ("gt",[arg1;arg2]) ->
        if inv then fun label -> ("",BobCode.bleq (arg1,arg2,label))
        else fun label -> ("",BobCode.bgt (arg1,arg2,label))
    | ("geq",[arg1;arg2]) ->
        if inv then fun label -> ("",BobCode.blt (arg1,arg2,label))
        else fun label -> ("",BobCode.bgeq (arg1,arg2,label))
    | _ -> failwith (sprintf "not yet traverseCompOp '%s'" op)
(* Clear global state in all module.
 * This includes .NET data structs and so on.
 * Clearing is important since global values are
 * treated as static
 * *)
let clear () =
    clearGs()

let compile tree = (traverse tree,warnings)
