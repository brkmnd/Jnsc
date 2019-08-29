module JanusAbSyn
(* Types for the abstract syntax tree
 *    Variations in int*int tuples are positions.
 *    One should be present for each assertions, ex.
 *    2 for if-statements, one for if and one for fi
 * *)
type Prg =
    | Procs of Stmt list
    | Error
and Stmt =
    | Proc of string * (Atom list) * (Stmt list)
    | BindOp of string * Atom * Expr * (int * int)
    | Call of string * (Atom list) * (int * int)
    | Uncall of string * (Atom list) * (int * int)
    | CallExt of string * (Atom list) * (int * int)
    | Decs of Atom list
    | If of Expr * (Stmt list) * (Stmt list) * Expr * ((int * int) * (int * int))
    | From of Expr * (Stmt list) * (Stmt list) * Expr * ((int * int) * (int * int))
    | Local of Atom * Expr * (Stmt list) * Atom * Expr * ((int * int) * (int * int))
    | Skip
    with
    static member toString (s) =
        match s with
        | BindOp (op,l,r,_) ->
            let ops =
                if op = "switch" then "&lt;=&gt;"
                else sprintf "%s=" (Expr.op2sym op)
            sprintf
                "%s %s %s"
                (Atom.toString l)
                ops
                (Expr.toString r)
        | _ -> ""
and Expr =
    | BinOp of string * Expr * Expr * (int * int)
    | UnaOp of string * Expr * (int * int)
    | Literal of Atom
    with
    static member op2sym (op) =
        match op with
        | "plus"    -> "+"
        | "minus"   -> "-"
        | "times"   -> "*"
        | "div"     -> "/"
        | "perc"    -> "%"
        | "up"      -> "^"
        | "ampamp"  -> "&&"
        | "eq"      -> "="
        | "neq"     -> "!="
        | "gt"      -> "&gt;" //">"
        | "lt"      -> "&lt;" //"<"
        | _ -> op
    static member isLitOne (a) =
        match a with
        | Literal v -> Atom.isValOne (v)
        | _ -> false
    static member isLit (a) =
        match a with
        | Literal _ -> true
        | _ -> false
    static member isLitId (a) =
        match a with
        | Literal (Id _) -> true
        | _ -> false
    static member isLitVal (v) =
        match v with
        | Literal (Val _) -> true
        | _ -> false
    static member toString(expr) =
        match expr with
        | BinOp (op,arg1,arg2,_) ->
            sprintf
                "(%s %s %s)"
                (Expr.op2sym op)
                (Expr.toString arg1)
                (Expr.toString arg2)
        | UnaOp (op,arg,_) ->
            sprintf
                "(%s %s)"
                (Expr.op2sym op)
                (Expr.toString arg)
        | Literal atom -> Atom.toString atom
    static member (+) (e : Expr,v : int) =
        BinOp ("plus",e,Literal (Val "1"),(-1,-1))
and Atom =
    | Id of DataType * DataAtts * string * (int * int)
    | Val of string
    | Index of Atom * Expr * (int * int)
    | CallBuildin of string * (Atom list) * (int * int)
    | Nil
    with
    static member (+) (arg1,arg2) =
        match (arg1,arg2) with
        | (Val l,Val r) ->
            let valL = (int) l
            let valR = (int) r
            let res = string (valL + valR)
            Val res
        | _ -> failwith "wrong type of args given to (+) in Atom"
    static member (-) (arg1,arg2) =
        match (arg1,arg2) with
        | (Val l,Val r) ->
            let valL = (int) l
            let valR = (int) r
            let res = string (valL - valR)
            Val res
        | _ -> failwith "wrong type of args given to (-) in Atom"
    static member (*) (arg1,arg2) =
        match (arg1,arg2) with
        | (Val l,Val r) ->
            let valL = (int) l
            let valR = (int) r
            let res = string (valL * valR)
            Val res
        | _ -> failwith "wrong type of args given to (*) in Atom"
    static member toString (a) =
        match a with
        | Id (_,_,id,_) -> id
        | Val v -> v
        | Index (a,ind,_) -> sprintf "%s[%s]" (Atom.toString a) (Expr.toString ind)
        | CallBuildin (id,args,_) ->
            sprintf "%s(%s)" id (String.concat "," (List.map Atom.toString args))
        | _ -> failwith (sprintf "'%A' not yet in Atom.toString" a)
    static member isValOne (a) =
        match a with
        | Val "1" -> true
        | _ -> false
    static member isVal (a) =
        match a with
        | Val _ -> true
        | _ -> false
    static member isIndex (a) =
        match a with
        | Index _ -> true
        | _ -> false
    static member filterType (a) =
        match a with
        | Id (t,_,_,_) -> t
        | Val _ -> Var
        | Index _ -> Array Var
        | CallBuildin _ -> Var
        | Nil -> Empty
    static member filterTypes (l) =
        match l with
        | [] -> Empty
        | [a] -> Atom.filterType a
        | a::al -> Product (Atom.filterType a,Atom.filterTypes al)
and DataAtts = {
    size : int
    }
and DataType =
    | Int
    | Bool
    | Stack
    | Array of DataType
    | Var
    | Product of DataType * DataType
    | Empty
    with
    static member toString = function
        | Int -> "int"
        | Stack -> "stack"
        | Array t -> (DataType.toString t) + " array"
        | Var -> "@"
        | Bool -> "bool"
        | Product (t1,t2) -> sprintf "%s*%s" (DataType.toString t1) (DataType.toString t2)
        | Empty -> "unit"
    static member isBool = function
        | Bool -> true
        | _ -> false
    static member isInt = function
        | Int -> true
        | _ -> false
    static member isVar = function
        | Var -> true
        | _ -> false
    static member isArray = function
        | Array _ -> true
        | _ -> false
    static member unfold = function
        | Array t -> t
        | t -> t
    static member (==) (t1,t2) =
        match (t1,t2) with
        | (Int,Int) -> true
        | (Stack,Stack) -> true
        | (Array at1,Array at2) -> at1 == at2
        | (Product (t11,t12),Product (t21,t22)) ->
            t11 == t12 && t12 == t22
        | (Var,Var) -> true
        | (Empty,Empty) -> true
        | _ -> false
    static member (!=) (t1 : DataType,t2 : DataType) =
        not (t1 == t2)
