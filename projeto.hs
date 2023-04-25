import System.IO



--Tarefa numero 1, dar print a disciplina e nome de alunos inscritos na mesma
tarefa1 :: IO()
tarefa1 = do
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"
    printDisciplinas (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)

    


    
printDisciplinas :: [String] -> [String] -> [String]-> IO() -- da print apenas no nome da uc
printDisciplinas [] y x = return ()
printDisciplinas (linha:linhas) conteudo_insc conteudo_alunos= do

    putStrLn ("-----"++ unwords(tail(tail (words linha))) ++ "-----") -- dar print ao nome da disciplina
    let numero = head (words linha) --numero da disciplina

    descobrirAlxxx numero conteudo_insc conteudo_alunos  -- descobrir al usando numero
    printDisciplinas linhas conteudo_insc conteudo_alunos -- print proxima disciplina


descobrirAlxxx:: String -> [String] -> [String]-> IO()
descobrirAlxxx numero [] conteudo_alunos = return()
descobrirAlxxx numero (linha:linhas) conteudo_alunos = do
    let numero_al = head (words linha)
    if last (words linha) == numero
        then descobrirNome numero_al conteudo_alunos
        else return ()
    descobrirAlxxx numero linhas conteudo_alunos

descobrirNome :: String -> [String] -> IO()
descobrirNome al [] = return()
descobrirNome al (linha:linhas) = do
    if head (words linha) == al
        then putStrLn (unwords (tail (tail(words linha))))
        else return()
    descobrirNome al linhas

    
    
    
    
    
    
