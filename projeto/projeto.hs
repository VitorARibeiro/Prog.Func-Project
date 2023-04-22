import System.IO

-- le o conteudo do arquivo e faz print do mesmo 
imprimeArquivo :: String -> IO ()
imprimeArquivo nomeArquivo = do
    conteudo <- readFile nomeArquivo
    putStrLn conteudo

-- le o conteudo das ucs, incriçoes e lista de alunos
lerArquivos :: IO (String, String, String)
lerArquivos = do
    conteudoUCS <- readFile "ucs.txt"
    conteudoInscricoes <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"
    return (conteudoUCS, conteudoInscricoes, conteudoAlunos)

-- Oapresenta os alunos inscritos numa respetiva unidade curicular
alunosPorUC :: String -> [String] -> [String]
alunosPorUC uc = filter (\linha -> head (words linha) == uc)

-- faz uma listagem dos alunos inscritos em cada unidade curricular
listarAlunosPorUCs :: IO ()
listarAlunosPorUCs = do
    (conteudoUCS, conteudoInscricoes, _) <- lerArquivos
    let ucs = lines conteudoUCS
        inscricoes = lines conteudoInscricoes
        alunosPorUCs = map (\uc -> (uc, alunosPorUC uc inscricoes)) ucs
        -- map é a funçao aplciada em cada elemento da lista 
    mapM_ (\(uc, alunos) -> do
        -- mapM é a lista em si ou seja executa uma determinada açao em cada elemento da lista
        -- e retorna o resultado de todas as açoes juntas
        putStrLn $ "Unidade Curricular: " ++ uc
        putStrLn "Alunos:"
        putStrLn $ unlines alunos) alunosPorUCs


