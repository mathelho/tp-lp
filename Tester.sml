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
run (fromString "var func1 = fn (Int x) => 2*x end; var func2 = fn (Int x) => 3*x end; var funcList = (func1, func2); var myF = funcList[1]; myF(5)");