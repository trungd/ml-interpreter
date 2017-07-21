%{
open Syntax
open Typing
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MINUS MULT LT GT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ REC ANDKW
%token RARROW FUN DFUN
%token COMMENT
%token MATCH EMPTYLIST WITH TWOCOLONS SEP
%token LSQBRACKET RSQBRACKET SEMI
%token COLON SINGQUO TYPE_BOOL TYPE_INT

%token <int> INTV
%token <Syntax.id> ID
%token <Syntax.id> TYPE_ID

%start toplevel
%type <Syntax.program> toplevel
%%

IdWithTypeExpr:
| x=ID COLON ty=TypeExpr { (x, ty) }
| x=ID { (x, TyNone) }

TypeAExpr:
| TYPE_INT { TyInt }
| TYPE_BOOL { TyBool }
| x=TYPE_ID { TyVar (TyVarName x) }

TypeFunExpr: 
| t1=TypeAExpr RARROW t2=TypeAExpr { TyFun(t1, t2) }

TypeExpr:
| LPAREN t=TypeExpr RPAREN { t }
| t=TypeAExpr { t }
| LPAREN t=TypeFunExpr RPAREN { t }

(* Ex 3.3.4: multiple declaration with and *)
LetAndExpr :
| x=IdWithTypeExpr EQ e=Expr ANDKW ls=LetAndExpr { (x,e)::ls }
| x=IdWithTypeExpr EQ e=Expr { [(x ,e)] }
| x=IdWithTypeExpr param=IdWithTypeExpr EQ e=Expr { [(x, FunExp([param], TyNone, e))] }
| x=IdWithTypeExpr param=IdWithTypeExpr COLON t=TypeExpr EQ e=Expr { [(x, FunExp([param], t, e))] }
(* /Ex3.3.4 *)

LetRecAndExpr :
| x=IdWithTypeExpr param=IdWithTypeExpr EQ e=Expr ANDKW ls=LetRecAndExpr { (x, param, e)::ls }
| x=IdWithTypeExpr EQ FUN param=IdWithTypeExpr RARROW e=Expr ANDKW ls=LetRecAndExpr { (x, param, e)::ls }
| x=IdWithTypeExpr param=IdWithTypeExpr EQ e=Expr { [(x, param, e)] }
| x=IdWithTypeExpr EQ FUN param=IdWithTypeExpr RARROW e=Expr { [(x, param, e)] }

TopLetExpr :
| LET x=IdWithTypeExpr EQ e=Expr ls=TopLetExpr { (x, e)::ls }
| LET x=IdWithTypeExpr EQ e=Expr { [(x, e)] }
| LET x=IdWithTypeExpr ls_param=FunParamListExpr EQ e=Expr { [(x, FunExp(ls_param, TyNone, e))] }
| LET x=IdWithTypeExpr ls_param=FunParamListExpr COLON t=TypeExpr EQ e=Expr { [(x, FunExp(ls_param, t, e))] }

TopLetRecExpr :
| LET REC x=IdWithTypeExpr param=IdWithTypeExpr EQ e=Expr ls=TopLetRecExpr { (x, param, e)::ls }
| LET REC x=IdWithTypeExpr EQ FUN param=IdWithTypeExpr RARROW e=Expr ls=TopLetRecExpr { (x, param, e)::ls }
| LET REC x=IdWithTypeExpr param=IdWithTypeExpr EQ e=Expr { [(x, param, e)] }
| LET REC x=IdWithTypeExpr EQ FUN param=IdWithTypeExpr RARROW e=Expr { [(x, param, e)] }

toplevel :
| e=Expr SEMISEMI { Exp e }
| LET REC ls=LetRecAndExpr SEMISEMI { RecDecls ls }
| ls=TopLetExpr SEMISEMI { Decls ls }
| LET ls=LetAndExpr SEMISEMI { AndDecls ls }
| ls=TopLetRecExpr SEMISEMI { RecDecls ls }

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
| e=AppendExpr { e }
| e=ListExpr { e }
| e=MatchExpr { e }

LTExpr : 
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | LT { OpFunExp (Lt) }
  | e=GTExpr { e }

GTExpr : 
    l=PExpr GT r=PExpr { BinOp (Gt, l, r) }
  | GT { OpFunExp (Gt) }
  | e=EQExpr { e }

EQExpr : 
    l=PExpr EQ r=PExpr { BinOp (Eq, l, r) }
  | EQ { OpFunExp (Eq) }
  | e=PExpr { e }

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
  | e=AExprEx { e }

(* a::[] *)
AppendExpr :
  | e1=Expr TWOCOLONS e2=Expr { AppendExp(e1, e2) }

AppParamListExpr :
    e=AExpr ls=AppParamListExpr  { e::ls }
  | e=AExpr { [e] }

AExpr :
| i=INTV { ILit i }
| TRUE   { BLit true }
| FALSE  { BLit false }
| i=ID   { Var i }
| LPAREN e=Expr RPAREN { e }
| e=ListExpr { e }

AExprEx :
| i=INTV { ILit i }
| TRUE   { BLit true }
| FALSE  { BLit false }
| i=ID   { Var i }
| LPAREN e=Expr RPAREN { e }
| e=IfExpr { e }
| e=LetExpr{ e }  
| e=LetRecExpr { e }
| e=FunExpr { e }
| e=DFunExpr { e }
| e=ListExpr { e }
| e=MatchExpr { e }

IfExpr :
  | IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LetExpr :
| LET ls=LetAndExpr IN e=Expr { LetExp(ls, e) }
| LET x=IdWithTypeExpr ls_param=FunParamListExpr EQ e=Expr IN e2=Expr { LetExp([(x, FunExp(ls_param, TyNone, e))], e2) }
| LET x=IdWithTypeExpr ls_param=FunParamListExpr COLON t=TypeExpr EQ e=Expr IN e2=Expr { LetExp([(x, FunExp(ls_param, t, e))], e2) }

LetRecExpr :
| LET REC ls=LetRecAndExpr IN e=Expr { LetRecExp(ls, e) }

FunExpr :
| FUN ls_param=FunParamListExpr COLON t=TypeExpr RARROW e=Expr { FunExp(ls_param, t, e) }
| FUN ls_param=FunParamListExpr RARROW e=Expr { FunExp(ls_param, TyNone, e) }

DFunExpr :
| DFUN x=ID RARROW e=Expr { DFunExp(x, e) }

FunParamListExpr :
| id=ID ls_param=FunParamListExpr { (id, TyNone) :: ls_param }
| id=ID { [(id, TyNone)] }
| LPAREN id_type=IdWithTypeExpr RPAREN ls_param=FunParamListExpr { id_type :: ls_param }
| LPAREN id_type=IdWithTypeExpr RPAREN { [id_type] }

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