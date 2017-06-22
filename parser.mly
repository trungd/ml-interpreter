%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT GT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ REC
%token RARROW FUN
%token COMMENT

%token <int> INTV
%token <Syntax.id> ID
%token <Syntax.idlist> IDLIST
%token <Syntax.binOp> BINOP

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) } 
  | LET REC x=ID para=ID EQ e=Expr SEMISEMI { RecDecl(x, para, e) }

Expr :
    e=IfExpr { e }
  | e=LetExpr{ e }  
  | e=LTExpr { e }
  | e=GTExpr { e }
  | e=EQExpr { e }
  | e=AndExpr { e }
  | e=OrExpr { e }
  | e=FunExpr { e }

LTExpr : 
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | LT { OpFunExp (Lt) }
  | e=PExpr { e }

GTExpr : 
    l=PExpr GT r=PExpr { BinOp (Gt, l, r) }
  | GT { OpFunExp (Gt) }
  | e=PExpr { e }

EQExpr : 
    l=PExpr EQ r=PExpr { BinOp (Eq, l, r) }
  | EQ { OpFunExp (Eq) }
  | e=PExpr { e }

PExpr :
  | l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | PLUS { OpFunExp (Plus) }
  | e=MExpr { e }

MExpr : 
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
  | MULT { OpFunExp (Mult) }
  | e=AppExpr { e }

(* Ex 3.2.3 *)
AndExpr:
    l=AExpr AND r=AExpr { BinOp(And, l, r) }
  | AND { OpFunExp (And) }

OrExpr:
    l=AExpr OR r=AExpr { BinOp(Or, l, r) }
  | OR { OpFunExp (Or) }
(**)

AppExpr :
  | e1=AppExpr e2=AExprListExpr { AppExp(e1, e2) } 
  | e=AExpr { e }

AExprListExpr :
    e=AExpr ls=AExprListExpr  { e::ls }
  | e=AExpr { [e] }

AExpr :
  | i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
  | IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LetExpr :
  | LET x=ID EQ e1=Expr IN e2=Expr { LetExp(x, e1, e2) }

LetRecExpr :
  | LET REC x=ID para=ID EQ e1=Expr IN e2=Expr { LetRecExp(x, para, e1, e2) }

IdListExpr :
  | id=ID idlist=IdListExpr { id :: idlist }
  | id=ID { [id] }

FunExpr :
  | FUN x=ID RARROW e=Expr { FunExp([x], e) }
  | FUN xlist=IdListExpr RARROW e=Expr { FunExp(xlist, e) }