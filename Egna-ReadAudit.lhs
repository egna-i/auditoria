>module Main where
>import Data.List (intersperse, elemIndex,sort,groupBy,sortBy,(\\))
>import System.Directory (doesFileExist, getHomeDirectory)
>import System.FilePath.Windows (combine)
>import System.Time (Day(..),TimeDiff(..),CalendarTime(..),Month(..), 
>                    toClockTime,addToClockTime,toUTCTime,getClockTime, diffClockTimes)

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

>keys = ["Termite log, started at ",
>        "NO. MAQUI",
>        "IMPRESSAO NUMERO",
>        "VALOR VENDAS",
>        "NUMERO VENDAS",
>        "DINH. NOS TUBOS",
>        "NO INTERR. DE AL.",
>        "TEMPO LIGACAO",
>        "DATA",
>        "IMPRESSAO NUMERO",
>        "VENDAS Moeda y C",
>        "NUMERO DE VENDAS",
>        "DESDE IMPRESSAO NO.",
>        "DINH. NO COFRE",
>        "RECAL",
>        "DINH. NOS TUBOS",
>        "TROCO DEVOLVIDO",
>        "INSERIDO MANUAL",
>        "DISP. MANUAL",
>        "VALOR DE VENDAS",
>        "NUMERO VENDAS",
>        "SOBREPAGO",
>        "FICHAS",
>        "VALUE TOKENS",
>        "NOTAS",
>        "CARTOES",
>        "RECARGA",
>        "VALORES PARCIAIS VENDAS",
>        "PARCIAL VENDAS GRATUITAS",
>        "NO VENDAS GRATUITAS",
>        "VAL. VENDAS GRATUIT",
>        "VENDAS POR CARTAO",
>        "VENDAS POR MOEDA",
>        "SEM VALOR DE VENDA"] ++
>        map show [1..100] ++
>        ["PRECOS ALTERADOS",
>         "------------------------",
>         "========================"]


>main :: IO ()
>main = do putStrLn version
>          return ()

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

