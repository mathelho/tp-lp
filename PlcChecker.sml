(* PlcChecker *)
(*Definir todas as funções de avaliação relacionadas ao tval
e ele inclusive, devem ficar aqui, e deixaremos somente o run para o Plc.sml*)
(*Ainda precisaremos lidar com as funções de avaliação
de vários tipos dentro de uma lista --- Pesquisar depois ---*)


exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

fun ehSequencia (SeqT t: plcType) = true
    |   ehSequencia _ = false;

fun supIgualdade IntT = true    (* verifica se o plcType suporta operações de igualdade*)
    |   supIgualdade BoolT = true
    |   supIgualdade (ListT []) = true
    |   supIgualdade (ListT (h::[])) = supIgualdade h
    |   supIgualdade (ListT (h::t)) = if supIgualdade h then supIgualdade(ListT t) else false
    |   supIgualdade (SeqT (t)) = supIgualdade t
    |   supIgualdade _ = false;

fun teval (e:expr) (p:plcType env): plcType =
    case e of
        (Var x) => lookup p x (*lookup vem do Envirom.sml*)
        | (ConI _) =>  IntT
        | (ConB _) =>  BoolT (*Bool está definido para TRUE e FALSE*)
        | (List []) => ListT [] (*O ListT tem que ser uma lista vazia também*)
        | (List l) =>
            let
                val lista = map(fn x => teval x p) l
            in
                ListT lista
            end
        | (ESeq (SeqT t)) => SeqT t
        | (ESeq _) => raise EmptySeq
        | (Let ((x:string), (e1:expr), (e2:expr))) =>
            let
                val t1 = teval e1 p
            in
                teval e2 ((x, t1)::p)
            end
        | (Letrec((f:string), (t:plcType), (x:string), (t1:plcType), (e1:expr), (e2:expr))) =>  (* verificar *)
            let
                val tipo1 = teval e1 ((f, FunT(t, t1))::(x, t)::p)
            in
                teval e2 ((f, FunT(t, t1))::p)
            end
        | (Anon((s:plcType), (x:string), (e:expr))) =>
            let
                val t = teval e ((x, s)::p)
            in
                FunT(s, t)
            end
        | (Call((e2:expr), (e1:expr))) =>
            let
                fun excecao (FunT(s, t)) = t
                |   excecao _ = raise NotFunc

                val t1 = teval e1 p
                val t2 = excecao(teval e2 p)
            in
                if teval e2 p = FunT(t1, t2) then t2 else raise CallTypeMisM
            end
        | (If((e:expr), (e1:expr), (e2:expr))) =>   (* verificar *)
            let
                val t = teval e p
                val t1 = teval e1 p
                val t2 = teval e2 p
            in
                if t <> BoolT then raise IfCondNotBool else if t1 = t2 andalso t2 = t then t else raise DiffBrTypes
            end
        | (Match((e:expr), l)) =>
            let
                val lista = map(fn (x, y) => ((teval (getOpt(x, (List []))) p), (teval y p))) l
                fun excecao [] p = raise NoMatchResults
                |   excecao ((ei, ri)::[]) p = if ei = (ListT []) then ri else if ei = teval e p then ri else raise MatchCondTypesDiff
                |   excecao ((ei, ri)::(en, rn)::t) p = if ei <> teval e p then raise MatchCondTypesDiff else if ri = rn then excecao((en, rn)::t) p else raise MatchResTypeDiff
            in
                excecao lista p
            end
        | (Prim1("!", (e:expr))) =>
            let
                val t1 = teval e p
            in
                if t1 = BoolT then BoolT else raise UnknownType
            end
        | (Prim1("-", (e:expr))) =>
            let
                val t1 = teval e p
            in
                if t1 = IntT then IntT else raise UnknownType
            end
        | (Prim1("hd", (e:expr))) =>    (* verificar *)
            let
                fun excecao (SeqT tipo) = tipo
                |   excecao _ = raise UnknownType
                val t1 = teval e p
            in
                if ehSequencia t1 then excecao t1 else raise UnknownType
            end
        | (Prim1("tl", (e:expr))) =>    (* verificar *)
            let
                val t1 = teval e p
            in
                if ehSequencia t1 then t1 else raise UnknownType
            end
        | (Prim1("ise", (e:expr))) =>   (* verificar *)
            let
                val t1 = teval e p
            in
                if ehSequencia t1 then BoolT else raise UnknownType
            end
        | (Prim1("print", (e:expr))) =>
            let
                val t1 = teval e p
            in
                ListT []
            end
        | (Prim2("&&", (e1:expr), (e2:expr))) =>
            let
                val t1 = teval e1 p
                val t2 = teval e2 p
            in
                if t1 = BoolT andalso t2 = BoolT then BoolT else raise UnknownType
            end
        | (Prim2("::", (e1:expr), (e2:expr))) =>
            let
                fun excecao (SeqT tipo) = tipo
                |   excecao _ = raise UnknownType
                val t1 = teval e1 p
                val t2 = teval e2 p
            in
                if ehSequencia t2 andalso t1 = excecao t2 then t2 else raise UnknownType
            end
        | (Prim2("+", (e1:expr), (e2:expr))) =>
            let
                val t1 = teval e1 p
                val t2 = teval e2 p
            in
                if t1 = IntT andalso t2 = IntT then IntT else raise UnknownType
            end
        | (Prim2("-", (e1:expr), (e2:expr))) =>
            let
                val t1 = teval e1 p
                val t2 = teval e2 p
            in
                if t1 = IntT andalso t2 = IntT then IntT else raise UnknownType
            end
        | (Prim2("*", (e1:expr), (e2:expr))) =>
            let
                val t1 = teval e1 p
                val t2 = teval e2 p
            in
                if t1 = IntT andalso t2 = IntT then IntT else raise UnknownType
            end
        | (Prim2("/", (e1:expr), (e2:expr))) =>
            let
                val t1 = teval e1 p
                val t2 = teval e2 p
            in
                if t1 = IntT andalso t2 = IntT then IntT else raise UnknownType
            end
        | (Prim2("<", (e1:expr), (e2:expr))) =>
            let
                val t1 = teval e1 p
                val t2 = teval e2 p
            in
                if t1 = IntT andalso t2 = IntT then BoolT else raise UnknownType
            end
        | (Prim2("<=", (e1:expr), (e2:expr))) =>
            let
                val t1 = teval e1 p
                val t2 = teval e2 p
            in
                if t1 = IntT andalso t2 = IntT then BoolT else raise UnknownType
            end
        | (Prim2("=", (e1:expr), (e2:expr))) =>
            let
                val t1 = teval e1 p
                val t2 = teval e2 p
            in
                if not (supIgualdade t1) orelse not (supIgualdade t2) then raise NotEqTypes else if t1 = t2 then BoolT else raise UnknownType
            end
        | (Prim2("!=", (e1:expr), (e2:expr))) =>
            let
                val t1 = teval e1 p
                val t2 = teval e2 p
            in
                if not (supIgualdade t1) orelse not (supIgualdade t2) then raise NotEqTypes else if t1 = t2 then BoolT else raise UnknownType
            end
        | (Item (i, List [])) => raise ListOutOfRange
        | (Item (0, List(h::t))) => teval h p
        | (Item (i, List(h::t))) => teval (Item (i - 1, (List t))) p
        | (Item (_, _)) => raise OpNonList
        | (Prim2(";", (e1:expr), (e2:expr))) =>
            let
                val t1 = teval e1 p
                val t2 = teval e2 p
            in
                t2
            end
        | _ => raise UnknownType;
