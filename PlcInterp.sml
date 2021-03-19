(* PlcInterp *)
(*A funcao de eval ficará inteiramente aqui, e usaremos
o run, que vai unir  tval e eval, no Plc.sml*)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (e:expr) (p:plcVal env): plcVal =
    case e of
        (ConI x) => (IntV x)
        | _ => raise Impossible;