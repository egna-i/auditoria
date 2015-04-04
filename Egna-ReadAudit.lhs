>module Main where
>import Data.List (intersperse, elemIndex,sort,groupBy,sortBy,(\\))
>import System.Directory (doesFileExist, getHomeDirectory)
>import System.FilePath.Windows (combine)
>import System.Time (Day(..),TimeDiff(..),CalendarTime(..),Month(..), 
>                    toClockTime,addToClockTime,toUTCTime,getClockTime, diffClockTimes)

>import Text.Parsec

>import Egna.Audit
>import Egna.Table

>version = "Egna-ReadAudit " ++ "version 1.0.9.0"

>type Property = (String, String)

>showTable p (Table h rs) = (concat $ intersperse (cellSeparator p) $ map show h) ++ (lineSeparator p)
>                        ++ (concat $ intersperse (lineSeparator p) $ map (\r -> concat $ intersperse (cellSeparator p) r) rs)

>getProperties = map (\x -> (read x) :: Property) . lines


>conditional p k f l = if read $ getValue p k "True" then f l else l

>cellSeparator p = getValue p "output.table.cell.separator" "; "
>lineSeparator p = getValue p "output.table.line.separator" "\n"

>isAuditTrash '\160' = True
>isAuditTrash '\NUL' = True
>isAuditTrash '\9632' = True
>isAuditTrash _ = False

>createMissingAudit l = let x1 = map (audit_global_impressao_numero . audit_global) l
>                       in l ++ [ create_audit x3 [] | x3 <- [1 .. (maximum x1)] \\ x1]
>createMissingItem n = AuditItem n 0 (Money 0) (Money 0)

>filterDuplicates l =  map (last . sort)
>                    $ groupBy (\a b -> (audit_partial_desde_impressao_numero $ audit_partial a) > 0 &&
>                                       (audit_partial_desde_impressao_numero $ audit_partial a) == 
>                                       (audit_partial_desde_impressao_numero $ audit_partial b)) l


>workflow props input = showTable props 													-- criar CSV output
>                      $ getTableFromRows 													-- criar Table
>                      $ map (table_audit props) 											-- criar Rows
>                      $ conditional props "workflow.sort" sort                  			-- ordenar as Auditorias pelo numero
>                      $ filter ((>0) . audit_global_impressao_numero . audit_global) 		-- filtrar auditorias com o numero 0
>--                      $ conditional props "workflow.creategroup" (createGroupAudit props)	-- eliminar audits 
>                      $ conditional props "workflow.filterduplicates" filterDuplicates		-- eliminar audits 
>                      $ conditional props "workflow.createmissing" createMissingAudit		-- criar Audit em falta
>                      $ map (create_audit 0)												-- criar Audit
>                      $ filter (isAuditHeader props) 										-- filtar as secções inválidas
>                      $ map (audits isAuditSubSection) 									-- criação de subsecções
>                      $ audits isAuditSection 												-- criação de secções (auditorias)
>                      $ lines 																-- cria listas com as linhas
>                      $ filter (not . isAuditTrash) input 									-- remove os carateres com lixo

assimilar [] = []
assimilar (x:[]) = x
assimilar (x:y:xs) | length x == 1 && length y /= 1 = (head x:y) ++ assimilar xs
                   | otherwise = assimilar (y:xs)


>getPropertiesPath :: FilePath -> IO FilePath
>getPropertiesPath filename = do userDirectory <- getHomeDirectory
>                                existFilenameOnUser <- doesFileExist $ combine userDirectory filename
>                                return (if existFilenameOnUser then (combine userDirectory filename) else filename)

>main :: IO ()
>main = do putStrLn version
>          beforeWorkFlowTime <- getClockTime
>          propertiesPath <- getPropertiesPath "Egna-ReadAudit.prop"
>          putStrLn ("Read " ++ propertiesPath)
>          propsRaw <- readFile propertiesPath
>          props <- return $ getProperties propsRaw
>          putStrLn ("Read " ++ getValue props "input.filename" "DUMP.log")
>          input <- readFile $ getValue props "input.filename" "DUMP.log"
>          output <- return $ workflow props input
>          putStrLn ("Write " ++ getValue props "output.filename" "DUMP.csv")
>          writeFile (getValue props "output.filename" "DUMP.csv") output
>          afterWorkFlowTime <- getClockTime
>          putStrLn ("Workflow " ++ (show $ tdSec $ diffClockTimes afterWorkFlowTime beforeWorkFlowTime) ++ " secs")
>          return ()

