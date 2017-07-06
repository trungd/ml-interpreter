%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MINUS MULT LT GT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ REC ANDKW
%token RARROW FUN DFUN
%token COMMENT
%token MATCH EMPTYLIST WITH TWOCOLONS SEP
%token LSQBRACKET RSQBRACKET SEMI

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

(* Ex 3.3.4: multiple declaration with and *)
LetAndExpr :
| x=ID EQ e=Expr ANDKW ls=LetAndExpr { (x,e)::ls }
| x=ID EQ e=Expr { [(x ,e)] }
(* /Ex3.3.4 *)

LetRecAndExpr :
  | x=ID param=ID EQ e=Expr ANDKW ls=LetRecAndExpr { (x, param, e)::ls }
  | x=ID param=ID EQ e=Expr { [(x, param, e)] }

TopLetExpr :
| LET x=ID EQ e=Expr ls=TopLetExpr { (x, e)::ls }
| LET x=ID EQ e=Expr { [(x, e)] }
| LET x=ID ls_param=FunParamListExpr EQ e=Expr { [(x, FunExp(ls_param, e))] }

toplevel :
| e=Expr SEMISEMI { Exp e }
| LET ls=LetAndExpr SEMISEMI { AndDecls ls }
| ls=TopLetExpr SEMISEMI { Decls ls }
| LET REC x=ID para=ID EQ e=Expr SEMISEMI { RecDecl(x, para, e) }

Expr :
| e=IfExpr { e }
| e=LetExpr{ e }  
| e=LetRecExpr { e }
| e=LTExpr { e }
| e=GTExpr { e }
| e=EQExpr { e }
| e=AndExpr { e }
| e=OrExpr { e }
| e=FunExpr { e }
| e=DFunExpr { e }
| e=ListExpr { e }
| e=MatchExpr { e }

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
  | e=MiExpr { e }

PExpr :
  | l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | PLUS { OpFunExp (Plus) }
  | e=MExpr { e }

MiExpr :
  | l=PExpr MINUS r=MExpr { BinOp (Minus, l, r) }
  | MINUS { OpFunExp (Minus) }
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
(* /Ex 3.2.3 *)

AppExpr :
  | e=AppExpr ls=AppParamListExpr { AppExp(e, ls) } 
  | e=AExpr { e }

AppParamListExpr :
    e=AExpr ls=AppParamListExpr  { e::ls }
  | e=AExpr { [e] }

AExpr :
  | i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }
  | exp=ListExpr { exp }

IfExpr :
  | IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LetExpr :
| LET ls=LetAndExpr IN e=Expr { LetExp(ls, e) }

LetRecExpr :
  | LET REC ls=LetRecAndExpr IN e=Expr { LetRecExp(ls, e) }

FunExpr :
| FUN ls_param=FunParamListExpr RARROW e=Expr { FunExp(ls_param, e) }

DFunExpr :
| DFUN x=ID RARROW e=Expr { DFunExp(x, e) }

FunParamListExpr :
| id=ID ls_param=FunParamListExpr { id :: ls_param }
| id=ID { [id] }

ListItemExpr :
  | exp=Expr SEMI ls=ListItemExpr { exp::ls }
  | exp=Expr { [exp] }

ListExpr :
  | LSQBRACKET ls=ListItemExpr RSQBRACKET { ListExp(ls) }
  | EMPTYLIST { ListExp ([]) }

MatchPatternExpr :
  | EMPTYLIST { EmptyList }
  | LSQBRACKET id=ID RSQBRACKET { SingleElementList(id) }
  | id1=ID TWOCOLONS id2=ID { ListHeadTail(id1, id2) }

MatchListExprs :
  | match_pattern=MatchPatternExpr RARROW exp=Expr SEP match_list=MatchListExprs { (match_pattern, exp) :: match_list }
  | match_pattern=MatchPatternExpr RARROW exp=Expr { [(match_pattern, exp)] }

MatchExpr :
  | MATCH exp=Expr WITH match_list=MatchListExprs { MatchExp(exp, match_list) }