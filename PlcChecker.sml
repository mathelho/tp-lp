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
        | (Match(e, l)) =>
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
                if t1 = InT then IntT else raise UnknownType
            end
        | (Prim1("hd", (e:expr))) =>    (* verificar *)
            let
                val t1 = teval e p
            in
                if t1 = SeqT then t1 else raise UnknownType
            end
        | (Prim1("tl", (e:expr))) =>    (* verificar *)
            let
                val t1 = teval e p
            in
                if t1 = SeqT then SeqT else raise UnknownType
            end
        | (Prim1("ise", (e:expr))) =>   (* verificar *)
            let
                val t1 = teval e p
            in
                if t1 = SeqT then BoolT else raise UnknownType
            end
        | (Prim1("print", (e:expr))) =>
            let
                val t1 = teval e p
            in
                ListT []
            end
