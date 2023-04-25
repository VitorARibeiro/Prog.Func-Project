import System.IO



--Tarefa numero 1, dar print a disciplina e nome de alunos inscritos na mesma
tarefa1 :: IO()
tarefa1 = do
    conteudoDisciplina <- readFile "ucs.txt"
    counteudoInscricao <- readFile "inscricoes.txt"
    printDisciplinas (lines conteudoDisciplina) (lines counteudoInscricao)
    


    
printDisciplinas :: [String] -> [String] -> IO() -- da print apenas no nome da uc
printDisciplinas [] y = return ()
printDisciplinas (linha:linhas) conteudo = do

    putStrLn (last  (words linha) ++ " :") -- dar print ao nome da disciplina


    let numero = head (words linha) --numero da disciplina
    descobrirAlxxx numero conteudo  -- descobrir al usando numero
    printDisciplinas linhas conteudo -- print proxima disciplina


descobrirAlxxx:: String -> [String] -> IO()
descobrirAlxxx numero [] = return ()
descobrirAlxxx numero (linha:linhas) = do
    if last (words linha) == numero
        then putStrLn (head (words linha))
        else return ()
    descobrirAlxxx numero linhas
    
    
    
    
    
    
