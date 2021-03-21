(* Infrastructure to run the Plc Front-End *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";
use "Parse.sml";
use "PlcChecker.sml";
use "PlcInterp.sml";
use "Plc.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

(* Teste do Monitor *)
val e = fromString "fun rec f1(Int x):Int = x + 1; f1(12)";
run e;

(*Nossos testes*)
(*Testes com o Run, com ambiente vazio, tirados do testParser,sml - Todos rodam; alguns cairam nas excecoes - SUCESSO*)
(*O run não recebe testes com ambiente não vazio, safe*)
run (fromString "15");
run (fromString "true");
run (fromString "()");
run (fromString "(6,false)[1]");
run (fromString "([Bool] [])");
run (fromString "print x");
run (fromString "3::7::t");
run (fromString "fn (Int x) => -x end");
run (fromString "var x = 9; x + 3");
run (fromString "fun f(Int x) = x; f(1)");
run (fromString "match x with | 0 -> 1| _ -> -1 end");

(*Alguns testes com o Teval - Todos rodam(menos o comentado), com ou sem ambiente vazio; alguns cairam nas excecoes - ...SUCESSO*)
teval (fromString "print x; false") [("x", BoolT)];
teval (fromString "-1") [];
teval (fromString "print x; y + 2") [("x", IntT), ("y", IntT)]; 
teval (fromString "3::7::t");
teval (fromString "(5,false)[0]") [];
teval (fromString "(5,false)[1]") [];
(*teval (fromString "(5,false)[2]") []; Não consegui implementar no Item essa excecao :*)

(*Testes com o Eval - Todos rodam, com ou sem ambiente vazio - SUCESSO*)
eval (fromString "false") [];
eval (fromString "-1") [];
eval (fromString "print x; true") [("x", BoolV false)];
eval (fromString "()") [];
eval (fromString "(6,false)[1]") [];
eval (fromString "([Bool] [])") [];
eval (fromString "3::7::([Int] [])") [];
eval (fromString "true::false::([Bool] [])") [];