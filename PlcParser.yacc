%%

%name PlcParser

%pos int

%term VAR
    | FUN | REC | DP
    | IF | THEN | ELSE | MATCH | WITH | EXCL | HEAD | TAIL | ISEMPTY | PRINT
    | AND | PLUS | MINUS | MULTI | DIV | EQ | DIF | LESS | LEQ | DDP | SEMIC
    | LBRACKET | RBRACKET | LCHAVE | RCHAVE | LPAR | RPAR
    | FN | END | SETAANON | VIRGULA | PIPE | SETA | UNDERLINE
    | NIL | BOOLEAN | INT
    | NAME of string | INTEGER of int | TRUE of bool | FALSE of bool
    | EOF

%nonterm Prog of expr | Decl of expr | Expr of expr | AtomExpr of expr | AppExpr of expr 
| Const of expr | Comps of expr list | MatchExpr of (expr option * expr) list | CondExpr of expr option
| Args of (plcType * string) list | Params of (plcType * string) list | TypedVar of plcType * string
| Type of plcType | AtomType of plcType | Types of plcType list

%right SEMIC SETA
%nonassoc IF
%left ELSE
%left AND
%left EQ DIF
%left LESS LEQ
%right DDP
%left PLUS MINUS
%left MULTI DIV
%nonassoc EXCL HEAD TAIL ISEMPTY PRINT NAME
%left LBRACKET

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
  |  Decl (Decl)

Decl : VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
  |  FUN NAME Args EQ Expr SEMIC Prog (Let(NAME, makeAnon(Args, Expr), Prog))
  |  FUN REC NAME Args DP Type EQ Expr SEMIC Prog (makeFun(NAME, Args, Type, Expr, Prog))

Expr : AtomExpr (AtomExpr)
  |  AppExpr (AppExpr)
  |  IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
  |  MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
  |  EXCL Expr (Prim1("!", Expr))
  |  MINUS Expr (Prim1("-", Expr))
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
  |  Expr LBRACKET INTEGER RBRACKET (Item(INTEGER, Expr))

AtomExpr : Const (Const)
  |  NAME (Var(NAME))
  |  LCHAVE Prog RCHAVE (Prog)
  |  LPAR Expr RPAR (Expr)
  |  LPAR Comps RPAR (List(Comps))
  |  FN Args SETAANON Expr END (makeAnon(Args, Expr))

AppExpr : AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
  |  AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const : TRUE (ConB(true))
  |  FALSE (ConB(false))
  |  INTEGER (ConI(INTEGER))
  |  LPAR RPAR (List [])
  |  LPAR Type LBRACKET RBRACKET RPAR (ESeq(Type))

Comps : Expr VIRGULA Expr (Expr1::Expr2::[])
  |  Expr VIRGULA Comps (Comps)

MatchExpr : END ([])
  |  PIPE CondExpr SETA Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr : Expr (SOME(Expr))
  |  UNDERLINE (NONE)

Args : LPAR RPAR ([])
  |  LPAR Params RPAR (Params)

Params : TypedVar (TypedVar::[])
  |  TypedVar VIRGULA Params (TypedVar::Params)

TypedVar : Type NAME (Type, NAME)

Type : AtomType (AtomType)
  |  LPAR Types RPAR (ListT(Types))
  |  LBRACKET Type RBRACKET (SeqT(Type))
  |  Type SETA Type (FunT(Type1, Type2))

AtomType : NIL (ListT [])
  |  BOOLEAN (BoolT)
  |  INT (IntT)
  |  LPAR Type RPAR (Type)

Types : Type VIRGULA Type (Type1::Type2::[])
  |  Type VIRGULA Types (Type::Types)
