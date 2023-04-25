import System.IO


-----Tarefas-----

--Tarefa1
tarefa1 :: IO()
tarefa1 = do
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"
    printDisciplinas (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)

--Tareda2
tarefa2 :: IO()
tarefa2 = do
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"
    printNomes (lines conteudoAlunos) (lines conteudoInscricao) (lines conteudoDisciplina)

tarefa3 :: IO()
tarefa3 = do
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"
    putStrLn "indique o nome da disciplina"
    disciplina <- getLine
    printDisciplinas_input disciplina (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)

-------Funcoes utilizadas nas tarefas-------
---Funcoes tarefa3 
printDisciplinas_input :: String -> [String] -> [String] -> [String]-> IO() -- da print apenas no nome da uc
printDisciplinas_input w [] y x = return ()
printDisciplinas_input input conteudo_disc conteudo_insc conteudo_alunos= do

    putStrLn ("-----"++ input ++ "-----") -- dar print ao nome da disciplina
    encontrarNome input conteudo_disc conteudo_insc conteudo_alunos


encontrarNome:: String -> [String] -> [String] -> [String]-> IO() --descobre o numero da disciplina a partir do nome
encontrarNome x [] y z = return()
encontrarNome input (linha:linhas) conteudo_insc conteudo_alunos = do
    let numero = head (words linha)
    if input == unwords (tail(tail(words linha)))
        then descobrirAlxxx numero conteudo_insc conteudo_alunos --funcao usada na tarefa 1, com o numero descobre o al
        else return()
    encontrarNome input linhas conteudo_insc conteudo_alunos


---Funcoes Tarefa2
printNomes :: [String] -> [String] -> [String]-> IO() 
printNomes [] y x = return ()
printNomes (linha:linhas) conteudo_insc conteudo_dis= do

    putStrLn ("-----"++ unwords(tail(tail (words linha))) ++ "-----") -- dar print ao nome da disciplina
    let numero_al = head (words linha) --numero da disciplina

   
    descobrirNumero numero_al conteudo_insc conteudo_dis  -- descobrir al usando numero
    printNomes linhas conteudo_insc conteudo_dis-- print proxima disciplina

descobrirNumero:: String -> [String] -> [String]-> IO() --descobre o al a partir do numero da disciplina
descobrirNumero numero_al [] conteudo_disc = return()
descobrirNumero numero_al (linha:linhas) conteudo_disc = do
    let numero_al2 = last (words linha) -- al do aluno
    if head (words linha) == numero_al
        then descobrirUc numero_al2 conteudo_disc --tranforma al em nome
        else return ()
    descobrirNumero numero_al linhas conteudo_disc

descobrirUc :: String -> [String] -> IO() --descobre o nome a partir do al
descobrirUc numero_al2 [] = return()
descobrirUc numero_al2 (linha:linhas) = do
    if head (words linha) == numero_al2
        then putStrLn (unwords (tail (tail(words linha)))) --dar print no nome
        else return()
    descobrirNome numero_al2 linhas

----Funcoes Tarefa1
printDisciplinas :: [String] -> [String] -> [String]-> IO() -- da print apenas no nome da uc
printDisciplinas [] y x = return ()
printDisciplinas (linha:linhas) conteudo_insc conteudo_alunos= do

    putStrLn ("-----"++ unwords(tail(tail (words linha))) ++ "-----") -- dar print ao nome da disciplina
    let numero = head (words linha) --numero da disciplina

    descobrirAlxxx numero conteudo_insc conteudo_alunos  -- descobrir al usando numero
    printDisciplinas linhas conteudo_insc conteudo_alunos -- print proxima disciplina


descobrirAlxxx:: String -> [String] -> [String]-> IO() --descobre o al a partir do numero da disciplina
descobrirAlxxx numero [] conteudo_alunos = return()
descobrirAlxxx numero (linha:linhas) conteudo_alunos = do
    let numero_al = head (words linha) -- al do aluno
    if last (words linha) == numero
        then descobrirNome numero_al conteudo_alunos --tranforma al em nome
        else return ()
    descobrirAlxxx numero linhas conteudo_alunos

descobrirNome :: String -> [String] -> IO() --descobre o nome a partir do al
descobrirNome al [] = return()
descobrirNome al (linha:linhas) = do
    if head (words linha) == al
        then putStrLn (unwords (tail (tail(words linha)))) --dar print no nome
        else return()
    descobrirNome al linhas

    
    
    
    
    
    
