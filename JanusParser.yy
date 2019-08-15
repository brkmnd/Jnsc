%{
open JanusAbSyn

(* parse-error function and global value to fetch error *)
let mutable ErrorContextDescriptor : string = ""

let parse_error_rich =
    Some (fun (ctxt: FSharp.Text.Parsing.ParseErrorContext<_>) ->
        ErrorContextDescriptor <-
            match ctxt.CurrentToken with
            | None -> "At beginning of input\n"
            | Some token -> sprintf "at token %A\n" token
        )

(* AUX functions *)
let add2prcs prg prgs =
    match prgs with
    | Procs prcs -> Procs (prg :: prcs)
    | Error -> Error
let add2decs v decs =
    match decs with
    | Decs d -> Decs (v :: d)
    | _ -> decs
let defDataAtts = {size = -1}

%}
/* Value Tokens */
%token<string*(int*int)> VAL_ID
%token<string*(int*int)> VAL_INT
/* Pars */
%token<int*int> LBRACKET RBRACKET
%token<int*int> LPAR RPAR
/* Operators */
%token<int*int> COMMA
%token<int*int> EQ NEQ LT GT LEQ GEQ
%token<int*int> PLUS MINUS TIMES UP
%token<int*int> PERC DIV AMP AMPAMP MID MIDMID
%token<int*int> IFF
/* Bind operators */
%token<int*int> BIND_MINUS
%token<int*int> BIND_PLUS
%token<int*int> BIND_UP
/* Keywords */
%token<int*int> INT STACK
%token<int*int> MAIN PROC SKIP NIL
%token<int*int> IF THEN ELSE FI
%token<int*int> FROM DO LOOP UNTIL
%token<int*int> PUSH POP
%token<int*int> LOCAL DELOCAL
%token<int*int> CALL UNCALL
%token<int*int> EMPTY TOP
%token<int*int> EOI
/* Buildin Procedures */
%token<int*int> SIZE
%token<int*int> SHOW

/* Precedence and so on */
%nonassoc BIND_UP BIND_PLUS BIND_MINUS
%left COMMA
%left AMPAMP MIDMID
%left EQ
%left NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIV
%left PERC
%left UP
%left MID
%left AMP
%left IFF


%start start_entry
%type<Prg> start_entry
%%
start_entry:
      prog EOI { $1 }
    ;
prog:
      p_main p { add2prcs $1 $2 }
    ;
d:
      x                     { $1 defDataAtts }
    | x LBRACKET c RBRACKET { fun t -> $1 {size = (int $3)} (Array t) }
    ;
t:
      INT       { Int }
    | STACK     { Stack }
    ;
p_main:
      PROC MAIN LPAR RPAR declare s_comp { Proc ("main",[],$5::$6) }
    ;
declare:
      /* empty */       { Decs [] }
    | INT d declare     { add2decs ($2 Int) $3 } 
    | STACK x declare   { Decs [] }
    ;
p:
      /* empty */                   { Procs [] }
    | PROC q LPAR p_args RPAR s_comp p   { add2prcs (Proc($2,$4,$6)) $7 }
    ;
p_args:
      t x_arg COMMA p_args  { ($2 $1)::$4 }
    | t x_arg               { [$2 $1] }
    ;
s_comp:
      s                     { $1 }
    | s s_comp              { $1 @ $2 }
s: 
      x bind_op e                        { [BindOp (fst $2,$1 defDataAtts Var,$3,snd $2)] }
    | x LBRACKET e RBRACKET bind_op e    {
        [BindOp (fst $5,Index ($1 defDataAtts (Array Var),$3,$2),$6,snd $5)]
        }
    | x IFF x                               {
        [BindOp ("switch",$1 defDataAtts Var,Literal ($3 defDataAtts Var),$2)]
        }
    | IF e THEN s_comp ELSE s_comp FI e  { [If ($2,$4,$6,$8,($1,$7))] }
    | FROM e DO s_comp LOOP s_comp UNTIL e            { [From ($2,$4,$6,$8,($1,$7))] }
    | LOCAL t x EQ e s_comp DELOCAL t x EQ e     {
        [Local ($3 defDataAtts $2,$5,$6,$9 defDataAtts $8,$11,($1,$10))]
        }
    | CALL q LPAR call_args RPAR            { [Call ($2,$4,$1)] }
    | UNCALL q LPAR call_args RPAR          { [Uncall ($2,$4,$1)] }
    | PUSH LPAR x COMMA x RPAR              { [] }
    | POP LPAR x COMMA x RPAR               { [] }
    | SHOW LPAR x RPAR                      { [CallExt ("show",[$3 defDataAtts Var],$1)] }
    | SKIP                                  { [Skip] }
    ;
call_args:
      x COMMA call_args     { [$1 defDataAtts Var] @ $3 }
    | x                     { [$1 defDataAtts Var] }
    ;
e:
      e_lit                 { $1 }
    | MINUS e               { UnaOp ("minus",$2,$1) }
    | e PLUS e              { BinOp ("plus",$1,$3,$2) }
    | e MINUS e             { BinOp ("minus",$1,$3,$2) }
    | e UP e                { BinOp ("up",$1,$3,$2) }
    | e TIMES e             { BinOp ("times",$1,$3,$2) }
    | e DIV e               { BinOp ("div",$1,$3,$2) }
    | e PERC e              { BinOp ("perc",$1,$3,$2) }
    | e AMP e               { BinOp ("amp",$1,$3,$2) }
    | e AMPAMP e            { BinOp ("ampamp",$1,$3,$2) }
    | e MID e               { BinOp ("mid",$1,$3,$2) }
    | e MIDMID e            { BinOp ("midmid",$1,$3,$2) }
    | e LT e                { BinOp ("lt",$1,$3,$2) }
    | e GT e                { BinOp ("gt",$1,$3,$2) }
    | e EQ e                { BinOp ("eq",$1,$3,$2) }
    | e NEQ e               { BinOp ("neq",$1,$3,$2) }
    | e LEQ e               { BinOp ("leq",$1,$3,$2) }
    | e GEQ e               { BinOp ("geq",$1,$3,$2) }
    ;
e_lit:
      LPAR e RPAR           { $2 }
    | c                     { Literal (Val $1) }
    | x                     { Literal ($1 defDataAtts Var) }
    | x LBRACKET e RBRACKET { Literal (Index ($1 defDataAtts (Array Var),$3,$2)) }
    | e_p_buildin           { Literal $1 }
    | NIL                   { Literal Nil }
e_p_buildin:
      SIZE LPAR x RPAR      { CallBuildin ("size",[$3 defDataAtts Var],$1) }
    | EMPTY LPAR x RPAR     { Nil }
    | TOP LPAR x RPAR       { Nil }
c:
      VAL_INT               { fst $1 }
    ;
bind_op:
      BIND_PLUS     { ("plus",$1) }
    | BIND_MINUS    { ("minus",$1) }
    | BIND_UP       { ("up",$1) }
    ;
x:
      VAL_ID    { fun atts t -> Id (t,atts,fst $1,snd $1) }
    ;
x_arg:
      VAL_ID                    { fun t -> Id (t,defDataAtts,fst $1,snd $1) }
    | VAL_ID LBRACKET RBRACKET  { fun t -> Id (Array t,defDataAtts,fst $1,snd $1) }
q:
      VAL_ID        { fst $1 }
    ;
%%
