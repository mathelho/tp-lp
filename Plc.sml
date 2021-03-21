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
(*Determinar se usaremos acentos ou não '.'
Não, usaremos, da erro nos caracteres. Testei aqui*)

(*Ele fala sobre como lidar com as Exceções na aula: 2020-10-05 DCC024 LP - Tratamento de Erros 6*)

fun run e = 
    let 
        val typeEvaluation = type2string(teval e [])
        val valueEvaluation = val2string(eval e [])
    in
        valueEvaluation ^ " : "^ typeEvaluation (*Especificando a saída conforme informado no fórum.*)
        (* Vamos fazer com string pro enquanto. Dps temos que arrumar uma conversão para string*)
    end
    handle
        EmptySeq => "EXCECAO: E uma sequencia vazia"
        | UnknownType => "EXCECAO: O tipo e desconhecido"
        | NotEqTypes => "EXCECAO: A igualdade nao funciona pois os tipos sao diferentes"
        | WrongRetType => "EXCECAO: O tipo de retorno esta incorreto"
        | DiffBrTypes => "EXCECAO: Os valores para o if-then-else estao diferentes"
        | IfCondNotBool => "EXCECAO: A expressao contida no 'if' não e do tipo Bool"
        | NoMatchResults => "EXCECAO: Nao ha resultados para fazer o o match"
        | MatchResTypeDiff => "EXCECAO: Os tipos dos resultados no match estao diferentes"
        | MatchCondTypesDiff => "EXCECAO: As expressoes para realizar o match sao de tipos diferentes da do match"
        | CallTypeMisM => "EXCECAO: Passando pra uma chamada de funcao um tipo diferente do qual ela suporta"
        | NotFunc => "EXCECAO: O valor nao e uma funcao"
        | ListOutOfRange => "EXCECAO: O indice esta fora do tamanho da lista"
        | OpNonList => "EXCECAO: Nao e possivel acessar um elemento em uma expressao que nao e uma lista"
        | Impossible => "EXCECAO: Impossivel realizar a acao"
        | HDEmptySeq => "EXCECAO: Nao e possível acessar a cabeça de uma sequencia vazia"
        | TLEmptySeq => "EXCECAO: Nao e possível acessar a cauda de uma sequencia vazia"
        | ValueNotFoundInMatch => "EXCECAO: O valor nao foi encontrado no match"
        | NotAFunc => "EXCECAO: O valor nao corresponde a uma funcao"
        (*Exceção que ele mencionou no fórum, do Environ.sml*)
        | SymbolNotFound => "EXCECAO: Algum simbolo nao fora encontrado"
        | _ => "EXCECAO: Erro Desconhecido"
    
    (*Verificar o arquivo Envirom.sml, acho que tem funções dele que usaremos, segundo a Doc*)