>module Main where
>import Data.List (intersperse, elemIndex,sort,groupBy,sortBy,isPrefixOf,(\\))
>import System.Directory (doesFileExist, getHomeDirectory)
>import System.FilePath.Windows (combine)
>import System.Time (Day(..),TimeDiff(..),CalendarTime(..),Month(..), 
>                    toClockTime,addToClockTime,toUTCTime,getClockTime, diffClockTimes)

--import Data.List.Split

>version :: String
>version = "Egna-ReadAudit " ++ "version 1.0.11.0"

>type Property = (String, String)
>getProperties = map (\x -> (read x) :: Property) . lines

>getValue p k v = maybe v id (lookup k p)
>conditional p k f l = if read $ getValue p k "True" then f l else l

>cellSeparator p = getValue p "output.table.cell.separator" "; "
>lineSeparator p = getValue p "output.table.line.separator" "\n"

>isAuditTrash '\160' = True
>isAuditTrash '\NUL' = True
>isAuditTrash '\9632' = True
>isAuditTrash _ = False

>getPropertiesPath :: FilePath -> IO FilePath
>getPropertiesPath filename = do userDirectory <- getHomeDirectory
>                                existFilenameOnUser <- doesFileExist $ combine userDirectory filename
>                                return (if existFilenameOnUser then (combine userDirectory filename) else filename)

>type ColumnName = String
>

>keys = [("Termite log, started at ", 1),
>        ("NO. MAQUI", 1),
>        ("IMPRESSAO NUMERO", 1),
>        ("VALOR VENDAS", 1),
>        ("NUMERO VENDAS", 1),
>        ("DINH. NOS TUBOS", 1),
>        ("NO INTERR. DE AL.", 1),
>        ("TEMPO LIGACAO", 1),
>        ("DATA", 1),
>        ("IMPRESSAO NUMERO", 1),
>        ("VENDAS Moeda y C", 1),
>        ("NUMERO DE VENDAS", 1),
>        ("DESDE IMPRESSAO NO.", 1),
>        ("DINH. NO COFRE", 1),
>        ("RECAL", 1),
>        ("DINH. NOS TUBOS", 1),
>        ("TROCO DEVOLVIDO", 1),
>        ("INSERIDO MANUAL", 1),
>        ("DISP. MANUAL", 1),
>        ("VALOR DE VENDAS", 1),
>        ("NUMERO VENDAS", 1),
>        ("SOBREPAGO", 1),
>        ("FICHAS", 1),
>        ("VALUE TOKENS", 1),
>        ("NOTAS", 1),
>        ("CARTOES", 1),
>        ("RECARGA", 1),
>        ("VALORES PARCIAIS VENDAS", 1),
>        ("PARCIAL VENDAS GRATUITAS", 1),
>        ("NO VENDAS GRATUITAS", 1),
>        ("VAL. VENDAS GRATUIT", 1),
>        ("VENDAS POR CARTAO", 1),
>        ("VENDAS POR MOEDA", 1),
>        ("SEM VALOR DE VENDA", 1)] ++
>        zip (map show [1..10]) [ 1 | n <- [1..100] ] ++
>        [("PRECOS ALTERADOS", 1)]

         "------------------------",
         "========================"]

>--emptyStatements = (zip [0 | n <- [1..]] . (\(k,_) -> k) . unzip) keys         --
         
>--adiciona uma statement ao indice
>addStatement :: [(String, Integer)] -> String -> [(String, Integer)]
>addStatement list statement = (statement, maybe 1 (+1) $ lookup statement list) : filter (\(k,_) -> statement /= k) list


>canAddStatement :: [(String, Integer)] -> [(String, Integer)] -> String -> Bool
>canAddStatement k list statement = maybe 1 id (lookup statement list) <= maybe 0 id (lookup statement k)

>--addStatements k st statement | length st > 0 && canAddStatement k (last st) statement = [addStatement (last st) statement]
>--                             | otherwise                                              = st ++ [addStatement [] statement]

--maybe (statement, 1) (\x -> ) v

>workflow props input = show $ lines $ filter (not . isAuditTrash) input

split :: [String] -> ([String], [[String]]) -> [[String]]
split [] (a,b) = (a:b)
split (x:xs) (a,b) = split xs ()

split :: [String] -> 

--myPrefixOf              :: (Eq a) => [a] -> [a] -> Bool
myPrefixOf [] list         =  []
myPrefixOf _  []        =  []
myPrefixOf (x:xs) yy | any (isPrefixOf x) yy =  (head $ filter (isPrefixOf x) yy) : (myPrefixOf xs yy)
                     | otherwise = myPrefixOf xs yy


>main :: IO ()
>main = do beforeWorkFlowTime <- putStrLn version >> getClockTime
>          propsRaw <- getPropertiesPath "Egna-ReadAudit.prop" >>= \propertiesPath -> (putStrLn ("Read " ++ propertiesPath) >> readFile propertiesPath)
>          props <- return $ getProperties propsRaw
>          input <- putStrLn ("Read " ++ getValue props "input.filename" "DUMP.log") >> (readFile $ getValue props "input.filename" "DUMP.log")
>          output <- return $ workflow props input
>          putStrLn ("Write " ++ getValue props "output.filename" "DUMP.csv") >> writeFile (getValue props "output.filename" "DUMP.csv") output
>          afterWorkFlowTime <- getClockTime
>          putStrLn ("Workflow " ++ (show $ tdSec $ diffClockTimes afterWorkFlowTime beforeWorkFlowTime) ++ " secs") >> return ()


          beforeWorkFlowTime <- getClockTime
          propertiesPath <- getPropertiesPath "Egna-ReadAudit.prop"
          putStrLn ("Read " ++ propertiesPath)
          propsRaw <- readFile propertiesPath
          props <- return $ getProperties propsRaw
          putStrLn ("Read " ++ getValue props "input.filename" "DUMP.log")
          input <- readFile $ getValue props "input.filename" "DUMP.log"
          output <- return $ workflow props input
          putStrLn ("Write " ++ getValue props "output.filename" "DUMP.csv")
          writeFile (getValue props "output.filename" "DUMP.csv") output
          afterWorkFlowTime <- getClockTime
          putStrLn ("Workflow " ++ (show $ tdSec $ diffClockTimes afterWorkFlowTime beforeWorkFlowTime) ++ " secs")
          return ()

