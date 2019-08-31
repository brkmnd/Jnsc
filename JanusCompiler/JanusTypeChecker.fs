module JanusTypeChecker
open System.Collections.Generic
open JanusAbSyn
type Procedure = {
    args : Atom list
    body : Stmt list
    }
(* Peform
 * - Arg type and return type of operators
 * - Arg type and return type of boolean operators
 * - Call sig of buildin procs
 * - Var is checked whether in scope
 * - Binding is checked that same id does not appear on right side
 * *)
(* AUX functions *)
let fail msg (y,x,cpos) =
    failwith (sprintf " at (%d,%d,%d) %s" y x cpos msg)

(* Error messages
 * *)
let fMsgCond given =
    sprintf "condition expects (bool) but is given (%s)" given
let fMsgBind op t =
    sprintf
        "binding with %s expects right hand side to be of type (int) but is given (%s)"
        (Expr.op2sym op)
        t
let fMsgBind1 op id0 =
    let idStr =
        match id0 with
        | Id (_,_,id,_) -> id
        | _ -> ""
    sprintf
        "binding with %s can't contain the same id '%s' on left and right hand side"
        (Expr.op2sym op)
        idStr
let fMsgArgs calleeSig callerSig id =
    let expct = DataType.toString calleeSig
    let given = DataType.toString callerSig
    sprintf "'%s' expects (%s) but is given (%s)" id expct given
let fMsgNotInScope id = sprintf "'%s' not in scope" id

(* Scope
 * Is instantiated
 * *)
type Scope () =
    let scope = new Dictionary<string,DataType>()
    let usedIds = new List<string>()
    let fMsgNonId = "[internal] non-id in scope"
    let mutable captureUsed = false
    member this.add (id : string,v) =
        if scope.ContainsKey (id) then
            scope.[id] <- v
        else scope.Add(id,v)
    member this.add (atom : Atom) =
        match atom with
        | Id (t,_,id,_) -> this.add(id,t)
        | _ -> fail (sprintf "%s" fMsgNonId) (-1,-1,-1)
    member this.add (atom : Atom,v) =
        match atom with
        | Id (_,_,id,_) -> this.add(id,v)
        | _ -> fail (sprintf "%s" fMsgNonId) (-1,-1,-1)
    member this.get (id) =
        if scope.ContainsKey(id) then scope.[id]
        else Empty
    member this.get (atom : Atom) =
        match atom with
        | Id (_,_,id,pos) ->
            if scope.ContainsKey(id) then this.get(id)
            else fail (fMsgNotInScope id) pos
        | _ -> fail (sprintf "%s" fMsgNonId) (-1,-1,-1)
    member this.getIgn (atom : Atom) =
        match atom with
        | Id (_,_,id,pos) when scope.ContainsKey(id) -> this.get(id)
        | _ -> Empty
    member this.contains (k) = scope.ContainsKey (k)
    member this.foreach (f) = for v in scope do (f v.Key v.Value)
    member this.getTypes (args) =
        // Get types from a list of Atoms
        // In here replace every id with type obtained from scope
        let rec exec = function
            | [] -> Empty
            | [id] when id|>Atom.isId -> this.get(id)
            | [a] -> Atom.filterType a
            | id::xs when id|>Atom.isId ->
                Product (this.get(id),exec xs)
            | a::xs ->
                Product (Atom.filterType a,exec xs)
        exec args
    member this.usedClear () =
        usedIds.Clear()
    member this.usedActivate () =
        captureUsed <- true
    member this.usedDeactivate () =
        captureUsed <- false
    member this.usedAdd (v) =
        if captureUsed then usedIds.Add(v)
        else ()
    member this.usedCheckClear (v) =
        let res = usedIds.Contains(v)
        this.usedClear()
        this.usedDeactivate()
        res
    member this.usedCheckClear (v : Atom) =
        match v with
        | Id (_,_,id,_) -> this.usedCheckClear (id)
        | _ -> this.usedClear(); this.usedDeactivate(); false

(* Procedures
 * keep track of type sig of operators and
 * user defined procedures
 * *)
type Procedures() =
    let ps = new Dictionary<string,Procedure>()
    static member getBuildin =
        let bps = new Dictionary<string,(DataType * DataType)>()
        //bps.Add("show",(Array Int,Empty))
        bps.Add("size",(Array Int,Int))
        fun (id) ->
            if bps.ContainsKey(id) then bps.[id]
            else fail (sprintf "trying to fetch buildin proc '%s' that does not exist" id) (-1,-1,-1)
    static member getOp (op) =
        match op with
        // arit unary operators
        | "minus1"  -> (Int,Int)
        // arit binary operators
        | "plus"
        | "minus"
        | "times"
        | "div"
        | "up"
        | "perc"     -> (Product (Int,Int),Int)
        // comp operators
        | "eq"
        | "neq"
        | "gt"
        | "geq"
        | "lt"
        | "leq"     -> (Product (Int,Int),Bool)
        // logical operators
        | "ampamp" 
        | "midmid"  -> (Product (Bool,Bool),Bool) 
        | _         -> (Empty,Empty)
    member this.add(id,proc) =
        if ps.ContainsKey(id) then ps.[id] <- proc
        else ps.Add(id,proc)
    member this.get(id,pos) =
        if ps.ContainsKey(id) then ps.[id]
        else fail (sprintf "trying to fetch procedure '%s' that is not yet declared" id) pos
    member this.get(id) = this.get(id,(-1,-1,-1))
    member this.foreach(f) = for p in ps do (f p.Key p.Value)


(* Pseudo Evaluation
 * Traverse AbSynTree buttom up and try to append types
 * to first leaves and then nodes
 * *)
let rec evalProc (proc : Procedure) (procs : Procedures) =
    let scope : Scope = evalArgs proc.args (new Scope())
    evalStmts proc.body scope procs
and evalStmts stmts scope procs =
    List.fold (fun _ stmt -> evalStmt stmt scope procs) () stmts
and evalStmt stmt scope (procs : Procedures) =
    match stmt with
    | Decs decs ->
        List.fold (fun _ dec -> scope.add(dec)) () decs
    | BindOp (op,id,e,pos) ->
        let tid = evalAtom id scope 
        let te =
            scope.usedActivate()
            evalExpr e scope
        if not (DataType.isInt te) then
            scope.usedClear()
            fail (fMsgBind op (DataType.toString te)) pos
        elif scope.usedCheckClear (id)  && DataType.isInt tid then
            fail (fMsgBind1 op id) pos
        else ()
    | Call (id,args,pos) | Uncall (id,args,pos) ->
        let calleeSig = Atom.filterTypes (procs.get(id,pos).args)
        let callerSig = scope.getTypes (args)
        if calleeSig = callerSig then ()
        else fail (fMsgArgs calleeSig callerSig id) pos
    | CallExt (id,args,pos) ->
        let callee = Procedures.getBuildin(id)
        let callerSig = scope.getTypes (args)
        let calleeSig = fst callee
        if calleeSig = callerSig then ()
        else fail (fMsgArgs calleeSig callerSig id) pos
    | Local (id1,e1,s,id2,e2,pos) ->
        let te1 = evalExpr e1 scope
        let old1 = scope.getIgn(id1)
        let toScope1 = scope.add(id1)
        let t1 = scope.get(id1)
        if t1 != te1 then
            let msg =
                sprintf
                    "local declaration expected %s but is given %s"
                    (DataType.toString t1)
                    (DataType.toString te1)
            fail msg (fst pos)
        elif Atom.toString id1 <> Atom.toString id2 then
            let msg =
                sprintf
                    "local/delocal does not match in variable names '%s'/'%s'"
                    (Atom.toString id1)
                    (Atom.toString id2)
            fail msg (fst pos)
        else
            let valS = evalStmts s scope procs
            let restateScope = scope.add(id1,old1)
            ()
    | If (e1,stmts1,stmts2,e2,pos) ->
        let te1 = evalExpr e1 scope
        let te2 = evalExpr e2 scope
        if not (DataType.isBool te1) then
            fail (fMsgCond (DataType.toString te1)) (fst pos)
        elif not (DataType.isBool te2) then
            fail (fMsgCond (DataType.toString te2)) (snd pos)
        else
            evalStmts stmts1 scope procs
            evalStmts stmts2 scope procs
    | From (e1,s1,s2,e2,pos) ->
        let te1 = evalExpr e1 scope
        let te2 = evalExpr e2 scope
        if not (DataType.isBool te1) then
            fail (fMsgCond (DataType.toString te1)) (fst pos)
        elif not (DataType.isBool te2) then
            fail (fMsgCond (DataType.toString te2)) (snd pos)
        else
            evalStmts s1 scope procs
            evalStmts s2 scope procs
    | _ ->
        // Type checker is not responsible for rest
        ()
and evalExpr expr scope =
    let fMsg op t1 t2 =
        sprintf "'%s' expects (%s) but is given (%s)"
            (Expr.op2sym op)
            (DataType.toString t1)
            (DataType.toString t2)
    match expr with
    | Literal arg -> evalAtom arg scope
    | BinOp (op,e1,e2,pos) ->
        let tops = Procedures.getOp(op)
        let te1 = evalExpr e1 scope
        let te2 = evalExpr e2 scope
        let tes = Product (te1,te2)
        if tes = (fst tops) then snd tops
        else fail (fMsg op (fst tops) tes) pos
    | UnaOp (op,e,pos) ->
        let tops = Procedures.getOp(op + "1")
        let te = evalExpr e scope
        if te = (fst tops) then snd tops
        else fail (fMsg op (fst tops) te) pos
and evalAtom atom (scope : Scope) =
    match atom with
    | Id (_,_,id,pos) ->
        if not (scope.contains (id)) then
            fail (fMsgNotInScope id) pos
        elif (scope.get(id))|>DataType.isArray then
            fail (sprintf "can't operate directly on array '%s'" id) pos
        else
            scope.usedAdd (id)
            scope.get(id)
    | Index (Id (_,_,id,_),e,pos) ->
        let te = evalExpr e scope
        if not (scope.contains (id)) then
            fail (fMsgNotInScope id) pos
        elif te != Int then
            fail (sprintf "index expects (int) expression but is given (%s)" (DataType.toString te)) pos
        else
            let tid = scope.get(id)
            if not (tid|>DataType.isArray) then
                fail (sprintf "can't index on '%s' of type (%s)" id (DataType.toString tid)) pos
            else DataType.unfold tid
    | CallBuildin (id,args,pos) ->
        let callee = Procedures.getBuildin(id)
        let callerSig = scope.getTypes (args)
        let calleeSig = fst callee
        if calleeSig = callerSig then snd callee
        else
            fail
                (sprintf "'%s' expects (%s) but is given (%s)"
                    id
                    (DataType.toString calleeSig)
                    (DataType.toString callerSig)
                    )
                pos
    | Nil -> Empty
    | _ ->
        // Rest is either var of something impossible
        Int
and evalArgs args scope =
    List.fold (fun (s : Scope) arg -> s.add(arg); s) scope args
let eval tree =
    let procedures =
        let f = function
            | Procs l -> l
            | _ -> []
        List.foldBack
            (fun prc (acc : Procedures) ->
                match prc with
                | Proc (id,args,body) ->
                    acc.add(id,{args=args;body=body}); acc
                | _ -> acc
                )
            (f tree)
            (new Procedures())
    // Eval each independently
    // Fetch needed typesignature from procs
    // if a call is reached
    procedures.foreach(fun id proc -> evalProc proc procedures)
    tree
