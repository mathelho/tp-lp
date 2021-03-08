%%

%name PlcParser

%pos int

%term VAR
    | FUN | REC | DP
    | IF | THEN | ELSE | MATCH | WITH | EXCL | TRACO | HEAD | TAIL | ISEMPTY | PRINT
    | AND | PLUS | MINUS | MULTI | DIV | EQ | DIF | LESS | LEQ | DDP | SEMIC
    | LBRACKET | RBRACKET | LCHAVE | RCHAVE | LPAR | RPAR
    | FN | END | SETAANON | VIRGULA | PIPE | SETA | UNDERLINE
    | NIL of unit
    | NAME of string | INTEGER of int | BOOLEAN of bool
    | EOF

%nonterm Prog of expr | Prog of decl | Decl of expr | Expr of expr | AtomExpr of expr       (?)
| AppExpr of expr | Const of expr | Comps of expr | MatchExpr of expr | CondExpr of expr
| Args of expr | Params of plcType | TypedVar of plcType | Type of plcType 
| AtomType of plcType | Types of plcType

%right SEMIC SETA DDP
%left ELSE AND EQ DIF LESS LEQ PLUS MINUS MULTI DIV LBRACKET

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
  |  Decl SEMIC Prog (Prog)

Decl : VAR NAME EQ Expr (Let(NAME, Expr, Prog))
  |  FUN NAME Args EQ Expr (Let(NAME, Anon(ListT, Expr), Expr))
  |  FUN REC NAME Args DP Type EQ Expr (Letrec(NAME, Type, NAME2, Type, Expr, Expr))    (?)

Expr : AtomExpr (AtomExpr)
  |  AppExpr (Call(AppExpr, AppExpr))       (dois AppExpr? oq fazer)
  |  IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
  |  MATCH Expr WITH MatchExpr Match(Expr, List)
  |  EXCL Expr (Prim1("!", Expr))
  |  TRACO Expr (Prim1("-", Expr))
  |  HEAD Expr (Prim1("hd", Expr))
  |  TAIL Expr (Prim1("tl", Expr))
  |  ISEMPTY Expr (Prim1("ise", Expr))
  |  PRINT Expr (Prim1("print", Expr))
  |  Expr AND Expr (Prim2("&&", Expr1, Expr2))
  |  Expr PLUS Expr (Prim2("+", Expr1, Expr2))
  |  Expr MINUS Expr (Prim2("-", Expr1, Expr2))
  |  Expr MULTI Expr (Prim2("*", Expr1, Expr2))
  |  Expr DIV Expr (Prim2("/", Expr1, Expr2))
  |  Expr EQ Expr (Prim2("=", Expr1, Expr2))
  |  Expr DIF Expr (Prim2("!=", Expr1, Expr2))
  |  Expr LESS Expr (Prim2("<", Expr1, Expr2))
  |  Expr LEQ Expr (Prim2("<=", Expr1, Expr2))
  |  Expr DDP Expr (Prim2("::", Expr1, Expr2))
  |  Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
  |  Expr LBRACKET INTEGER RBRACKET (Item(INTEGER))

AtomExpr : Const (Const)
  |  NAME (Var(NAME))
  |  LCHAVE Prog RCHAVE (Prog)
  |  LPAR Expr RPAR (Expr)
  |  LPAR Comps RPAR (Comps)
  |  FN Args SETAANON Expr END (Anon(Type, NAME, Expr))      (oq entraria no lugar de Type? ListT?)

AppExpr : AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
  |  AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const : BOOLEAN (ConB(BOOLEAN))
  |  INTEGER (ConI(INTEGER))
  |  LPAR NIL RPAR (List [])                     (?)
  |  LPAR Type LBRACKET RBRACKET RPAR (ESeq(SeqT))

Comps : Expr VIRGULA Expr (Expr)
  |  Expr VIRGULA Comps (Comps)

MatchExpr : END ([])
  |  PIPE CondExpr SETA Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr : Expr (Expr)
  |  UNDERLINE (NONE)     (???) (Está Correto!)

Args : LPAR NIL RPAR (ListT)
  |  LPAR Params RPAR (List)    (?)

Params : TypedVar (TypedVar)
  |  TypedVar VIRGULA Params (ListT)    (?) Duvida enviada

TypedVar : Type NAME (Var(NAME))

Type : AtomType (AtomType)
  |  LPAR Types RPAR (ListT(Types))
  |  LBRACKET Type RBRACKET (SeqT(Type))
  |  Type SETA Type (FunT(Type1, Type2))

AtomType : NIL (ListT(NIL))
  |  BOOLEAN (BoolT(BOOLEAN))
  |  INTEGER (IntT(INTEGER))
  |  LPAR Type RPAR (ListT(Type))

Types : Type VIRGULA Type (ListT)      (?)
  |  Type VIRGULA Types (ListT(ListT)) (?)