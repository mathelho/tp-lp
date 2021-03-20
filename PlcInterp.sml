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
        | (ConB x) => (BoolV x)
        | (ESeq x) => (SeqV [])
        | (Var x) =>
            let in 
                (lookup p x) handle SymbolNotFound => raise SymbolNotFound 
            end
        | (Let (x, e1, e2)) =>
            let
                val t = (x, eval e1 p)::p
            in
                eval e2 t
            end
        | (Letrec (f, t, x, t1, e1, e2)) =>
            let
                val t = (f, Clos(f, x, e1, p))::p
            in
                eval e2 t
            end
        | (If (e1, e2, e3)) =>
            let in
                case eval e1 p of BoolV true => eval e2 p
                | BoolV false => eval e3 p
                | _ => raise Impossible
            end
        | (Match (e1, l)) =>
            let
                val v = eval e1 p
                fun casamento (var, x::[]) p =
                    let in
                        case x of (SOME e2, e3) =>
                            if var = eval e2 p then e3 else raise ValueNotFoundInMatch
                        | (NONE, e3) => e3
                    end
                | casamento (var, x::xs) p =
                    let in
                        case x of (SOME e2, e3) =>
                            if var = eval e2 p then e3 else casamento (var, xs) p
                        | (NONE, e3) => raise Impossible
                    end
                | casamento (var, _) p = raise Impossible
            in
                eval (casamento (v, l) p) p
            end
        | (Call (e1, e2)) =>
            let
                fun argumentos (List (x::[])) = [eval x p]
                | argumentos (List (x::xs)) = [eval x p] @ argumentos (List xs)
                | argumentos (e3) = [eval e3 p]

                val p1 = [("$list", ListV (argumentos e2))] @ p
                val t = eval e1 p
            in
                case t of Clos(f, v, e, p2) =>
                    let
                        val p3 = eval e2 p
                        val p4 = (v, p3)::(f, t)::p2
                    in
                        eval e p4
                    end
                | _ => raise NotAFunc
            end
        | (List []) => ListV []
        | (List l) =>
            let
                fun percorrer (x::[]) = eval x p :: []
                | percorrer (x::xs) = eval x p :: percorrer xs
                | percorrer _ = raise Impossible
            in
                ListV (percorrer l)
            end
        | (Item (i, e)) =>
            let
                fun retornaElemento (i, []) = raise Impossible
                | retornaElemento (i, (x::[])) = if i = 1 then x else raise Impossible
                | retornaElemento (i, (x::xs)) = if i = 1 then x else retornaElemento(i - 1, xs)

                val t = eval e p
            in
                case t of ListV l => retornaElemento(i, l)
                |   SeqV x => retornaElemento(i, x)
                |   _ => raise Impossible
            end
        | (Anon (t, x, e)) => Clos("", x, e, p)
        | (Prim1 ("print", e)) =>
            let
                val t = eval e p
            in
                case t of IntV x =>
                    let
                        val t = IntV x
                        val vazio = print(val2string(t) ^ "\n")
                    in
                        ListV []
                    end
                | BoolV x =>
                    let
                        val t = BoolV x
                        val vazio = print(val2string(t) ^ "\n")
                    in
                        ListV []
                    end
                | SeqV x =>
                    let
                        val vazio = print(list2string(val2string, x) ^ "\n")
                    in
                        ListV []
                    end
                | ListV x =>
                    let
                        val vazio = print(list2string(val2string, x) ^ "\n")
                    in
                        ListV []
                    end
                | _ => raise Impossible
            end
        | _ => raise Impossible;