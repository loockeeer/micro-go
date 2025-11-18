%{
  open Lexing
  open Mgoast

  exception Error

  let rec all_lvalues_are_vars l acc =
  match l with
  | [] -> Some acc
  | {edesc=(Var i); _}::tail -> all_lvalues_are_vars tail (i::acc)
  | _ -> None
%}

%token <int64> INT
%token <string> IDENT
%token <string> STRING

%token PIPEPIPE AMPAMP EQ NEQ GT GE LT LE PLUS MINUS STAR SLASH PERCENT UMINUS BANG DOT

%left PIPEPIPE
%left AMPAMP
%left EQ NEQ GT GE LT LE
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc UMINUS BANG
%left DOT

%token LPAR RPAR BEGIN END SEMI COMMA

%token IF ELSE FMT PRINT SET DEFSET FOR VAR RETURN INCR DECR

%token TRUE FALSE NIL TINT TBOOL TSTRING

%token PACKAGE IMPORT TYPE STRUCT FUNC
%token EOF

%start prog
%type <Mgoast.program> prog
%type <Mgoast.instr>   instr
%type <Mgoast.seq>     block
%type <Mgoast.instr>   simple_instr
%type <Mgoast.instr>   instr_if
%type <Mgoast.ident>   ident
%type <Mgoast.expr>    expr
%type <Mgoast.expr>    lvalue
%%

ident:
  id = IDENT { { loc = $startpos, $endpos; id = id } }
;

mgotype:
  | STAR s=IDENT  { TStruct(s) }
  | TINT          { TInt }
  | TBOOL         { TBool }
  | TSTRING       { TString }
;

prog:
| PACKAGE main=IDENT SEMI decls=list(decl) EOF
    { if main="main" then (false, decls) else raise Error}
| PACKAGE main=IDENT SEMI IMPORT fmt=STRING SEMI decls=list(decl) EOF
    { if main="main" && fmt="fmt" then (true, decls) else raise Error} 
;

block_inner:
  | i=instr { [i] }
  | inner=block_inner SEMI tail=instr { tail::inner } 
;

block:
    BEGIN b=block_inner option(SEMI) END { List.rev b }
;

instr_if:
  | IF e=expr b=block {{iloc=$startpos,$endpos; idesc=If(e,b, [])}}
  | IF e=expr bthen=block ELSE belse=block SEMI {{iloc=$startpos,$endpos; idesc=If(e,bthen,belse)}}
  | IF e=expr bthen=block ELSE i=instr_if {{iloc=$startpos,$endpos; idesc=If(e, bthen, [i])}}
;

lvalue:
  | i = ident { { eloc=$startpos,$endpos; edesc=Var i } }
  | e = expr DOT i=ident { { eloc = $startpos, $endpos; edesc=Dot(e,i) } }
;

simple_instr:
  | lhs=separated_nonempty_list(COMMA,lvalue)  SET    rhs=separated_nonempty_list(COMMA,expr) { {iloc=$startpos,$endpos; idesc = Set (lhs,rhs)} }
  | lhs=separated_nonempty_list(COMMA,lvalue)  DEFSET rhs=separated_nonempty_list(COMMA,expr) { {iloc=$startpos,$endpos; idesc = 
    match all_lvalues_are_vars lhs [] with 
    | Some l -> Vars (l, None, rhs) 
    | _ -> raise Error} 
  }
  | e=expr INCR { {iloc=$startpos,$endpos;idesc=Inc e} }
  | e=expr DECR { {iloc=$startpos,$endpos;idesc=Dec e} }
  | e=expr { {iloc=$startpos,$endpos;idesc=Expr e} }
;

instr:
  | s = simple_instr { s }
  | b = block { {iloc=$startpos,$endpos;idesc=Block b} }
  | i = instr_if { i }
  | VAR l = separated_nonempty_list(COMMA, ident) t = option(mgotype) { {iloc=$startpos,$endpos; idesc=Vars (l, t, [])} }
  | VAR l = separated_nonempty_list(COMMA, ident) t = option(mgotype) SET e=separated_nonempty_list(COMMA,expr) { {iloc = $startpos,$endpos; idesc=Vars (l, t, e)} }
  | RETURN r = separated_list(COMMA, expr) { {iloc=$startpos,$endpos; idesc=Return r} }
  | FOR b = block { { iloc=$startpos,$endpos; idesc=For({eloc = $startpos,$endpos; edesc = Bool true}, b) } }
  | FOR e=expr b=block { {iloc=$startpos,$endpos; idesc = For(e, b) } }
  | FOR i1=option(simple_instr) SEMI e=expr SEMI i2=option(simple_instr) b=block { 
                  {
                    iloc=$startpos,$endpos;
                    idesc =
                      let body = match i2 with
                                | None -> b
                                | Some i -> b@[i]
                      in
                      match i1 with
                      | None -> For(e, body)
                      | Some i -> Block [ i; {iloc=$startpos,$endpos;idesc = For (e, body)} ]
                  } 
              }
;

args_typed:
  | v = separated_nonempty_list(COMMA, ident) t=mgotype  { (List.map (fun x -> x,t) v) }
;

args(sep):
  | l = separated_list(sep, args_typed) { List.flatten l }
;
function_return_type:
  | { [] }
  | t=mgotype { [t] }
  | LPAR tl=separated_nonempty_list(COMMA, mgotype) RPAR { tl }
;

decl:
  | TYPE id=ident STRUCT BEGIN fl=args(SEMI) END SEMI
    { Struct { sname = id; fields = fl; } }
  | FUNC i=ident LPAR v=args(COMMA) RPAR t=function_return_type b=block SEMI
    { Fun {fname=i;params=v;return=t;body=b} }
;

%inline binop:
  | STAR      { Mul }  
  | EQ        { Eq  }
  | NEQ       { Neq }
  | LT        { Lt  }
  | LE        { Le  }
  | GT        { Gt  }
  | GE        { Ge  }
  | PLUS      { Add }
  | MINUS     { Sub }
  | SLASH     { Div }
  | PERCENT   { Rem }
  | AMPAMP    { And }
  | PIPEPIPE  { Or  }

expr:
  | n=INT { { eloc=$startpos,$endpos; edesc=Int n } }
  | c=STRING { { eloc=$startpos,$endpos; edesc=String c } }
  | TRUE { { eloc=$startpos,$endpos; edesc=Bool true } }
  | FALSE { { eloc=$startpos,$endpos; edesc=Bool false } }
  | NIL { { eloc=$startpos,$endpos; edesc=Nil } }
  | LPAR e=expr RPAR { e }
  | i=ident { { eloc=$startpos,$endpos; edesc=Var i } }
  | e=expr DOT i=ident { { eloc=$startpos,$endpos; edesc=Dot(e,i) } }
  | FMT DOT PRINT LPAR args=separated_list(COMMA,expr) RPAR { {eloc=$startpos,$endpos;edesc=Print(args)} }
  | i=ident LPAR args=separated_list(COMMA,expr) RPAR { { eloc=$startpos,$endpos; edesc=Call(i,args) } }
  | BANG e=expr { { eloc=$startpos,$endpos; edesc=Unop(Not,e) } }
  | MINUS e=expr { { eloc=$startpos,$endpos; edesc=Unop(Opp,e) } } %prec UMINUS
  | e1=expr op=binop e2=expr { { eloc=$startpos,$endpos; edesc=Binop(op,e1,e2) } }
;
