(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

fun keyword (s, lpos, rpos) =
    case s of
        "var" => VAR (lpos, rpos)
        | "Bool" => BOOLEAN (s, lpos, rpos)
        | "else" => ELSE (lpos, rpos)
        | "end" => END (lpos, rpos)
        | "false" => BOOLEAN (s, lpos, rpos)
        | "fn" => FN (lpos, rpos)
        | "fun" => FUN (lpos, rpos)
        | "hd" => HEAD (lpos, rpos)
        | "if" => IF (lpos, rpos)
        | "Int" => INTEGER (strToInt(s), lpos, rpos)
        | "ise" => ISEMPTY (lpos, rpos)
        | "match" => MATCH (lpos, rpos)
        | "Nil" => NIL (s, lpos, rpos)
        | "print" => PRINT (s, lpos, rpos)
        | "rec" => REC (lpos, rpos)
        | "then" => THEN (lpos, rpos)
        | "tl" => TAIL (lpos, rpos)
        | "true" => BOOLEAN (s, lpos, rpos)
        | "with" => WITH (lpos, rpos)
        | "_" => UNDERLINE (lpos, rpos)
        | _ => NAME (s, lpos, rpos)

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

fun strToInt s =
    case Int.fromString s of
        SOME i => i
    |   NONE => raise Fail ("Could not convert string '" ^ s ^ "' to integer")

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha=[A-Za-z];
name=[a-zA-Z_][a-zA-Z_0-9]*;
nat=[0-9];
whitespace=[\ \t];
%%

\n => (lineNumber := !lineNumber + 1; lex());
{whitespace}+ => (lex());
{nat}+ => (INTEGER(strToInt(yytext), yypos, yypos));
{name} => (keyword(yytext, yypos, yypos));
"=" => (EQ(yypos, yypos));
":" => (DP(yypos, yypos));
"!" => (EXCL(yypos, yypos));
"-" => (NEGATIVO(yypos, yypos));
"&&" => (AND(yypos, yypos));
"+" => (PLUS(yypos, yypos));
"-" => (MINUS(yypos, yypos));
"*" => (MULTI(yypos, yypos));
"/" => (DIV(yypos, yypos));
"!=" => (DIF(yypos, yypos));
"<" => (LESS(yypos, yypos));
"<=" => (LEQ(yypos, yypos));
"::" => (DDP(yypos, yypos));
";" => (SEMIC(yypos, yypos));
"[" => (LBRACKET(yypos, yypos));
"]" => (RBRACKET(yypos, yypos));
"{" => (LCHAVE(yypos, yypos));
"}" => (RCHAVE(yypos, yypos));
"(" => (LPAR(yypos, yypos));
")" => (RPAR(yypos, yypos));
"=>" => (SETAANON(yypos, yypos));
"," => (VIRGULA(yypos, yypos));
"|" => (PIPE(yypos, yypos));
"->" => (SETA(yypos, yypos));
"_" => (UNDERLINE(yypos, yypos));
. => (error("\n***Lexer error: bad character ***\n"); raise Fail("Lexer error: bad character " ^ yytext));