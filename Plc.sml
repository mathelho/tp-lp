(* Plc interpreter main file *)
(*Colocar a funcao Run nesse arquivo*)
(*Run irá executar o tval e o eval; e irá retorna a string
contendo o valor e o tipo das variáveis analisadas.
Pensaremos em como tratar listas e multiplos valores e variáfve depois
Vamos começar por casos simples*)

(*O Disparo de Exceções deve ser feito aqui*)
(*Ele mencionou que a saída deve ser -- valor":"tipo no fórum*)
use "PlcChecker.sml";
use "PlcInterp.sml";
use "Environ.sml";
(*Determinar se usaremos acentos ou não '.'*)

fun run e = 
    let 
        val typeEvaluation = type2string(teval e [])
        val valueEvaluation = val2string(eval e [])
    in
        valueEvaluation ^ " : "^ typeEvaluation (*Especificando a saída conforme informado no fórum.*)
        (* Vamos fazer com string pro enquanto. Dps temos que arrumar uma conversão para string*)
    end
    handle
        EmptySeq => "É uma sequência vazia"
        | UnknownType => "O tipo é desconhecido"
        | NotEqTypes => "A igualdade não funciona pois os tipos são diferentes"
        | WrongRetType => "O tipo de retorno está incorreto"
        | DiffBrTypes => "Os valores para o if-then-else estão diferentes"
        | IfCondNotBool => "A expressão contida no 'if' não é do tipo Bool"
        | NoMatchResults => "Não há resultados para fazer o o match"
        | MatchResTypeDiff => "Os tipos dos resultados no match estão diferentes"
        | MatchCondTypesDiff => "As expressões para realizar o match são de tipos diferentes da do match"
        | CallTypeMisM => "Passando pra uma chamada de função um tipo diferente do qual ela suporta"
        | NotFunc => "O valor não é uma função"
        | ListOutOfRange => "O índice está fora do tamanho da lista"
        | OpNonList => "Não é possível acessar um elemento em uma expressão que não é uma lista"
        | Impossible => "Impossivel realizar a ação"
        | HDEmptySeq => "Não é possível acessar a cabeça de uma sequência vazia" (*Verificar do que se trata*)
        | TLEmptySeq => "Não é possível acessar a cauda de uma sequência vazia" (*Verificar do que se trata*)
        | ValueNotFoundInMatch => "O valor não foi encontrado no match"
        | NotAFunc => "O valor não corresponde a uma função"
        (*Exceção que ele mencionou no fórum, do Environ.sml*)
        | SymbolNotFound => "Algum simbolo não fora encontrado" (*Verificar do que se trata*)
    
    (*Verificar o arquivo Envirom.sml, acho que tem funções dele que usaremos, segundo a Doc*)