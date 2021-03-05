%%

%name PlcParser

%pos int

%term VAR
    | FUN | REC
    | IF | THEN | ELSE | MATCH | WITH | EXCL | TRACO | HEAD | TAIL | ISEMPTY | PRINT
    | AND | PLUS | MINUS | MULTI | DIV | EQ | DIF | LESS | LEQ | DP | SEMIC
    | LBRACKET | RBRACKET | LCHAVE | RCHAVE | LPAR | RPAR
    | FN | END | SETAANON | TRUE | FALSE | VIRGULA | MATCHOR | SETA | TRACO2
    | NIL | BOOL
    | NAME of string | INTEGER of int
    | EOF

%nonterm Prog of expr | Prog of decl | Decl of expr | Expr of expr | AtomExpr of expr
| AppExpr of expr | Const of expr | Comps of expr | MatchExpr of expr | CondExpr of expr
| Args of expr | Params of plcType | Type of plcType | AtomType of plcType | Types of plcType
| Nat of expr

%right SEMIC SETA DP
%left ELSE AND EQ DIF LESS LEQ PLUS MINUS MULTI DIV LBRACKET

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
  |  Decl SEMIC Prog (Let(NAME, Expr, Prog))

Expr : AtomExpr (AtomExpr)
  |  AppExpr
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
  |  Expr DP Expr (Prim2("::", Expr1, Expr2))
  |  Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
  |  Expr LBRACKET Nat RBRACKET (ESeq(Nat))

AtomExpr : Const (Const)
  |  NAME (Var(NAME))
  |  LCHAVE Prog RCHAVE (Prog)
  |  LPAR Expr RPAR (Expr)
  |  LPAR Comps RPAR (Comps)
  