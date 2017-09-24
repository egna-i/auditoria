module Audit2Json where
import Data.List
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath.Windows (combine)

version = "Egna-Audit2Json " ++ "version 2.0.0.0"

getValue p k v = maybe v id (lookup k p)

type Property = (String, String)
getProperties = map (\x -> (read x) :: Property) . lines

getPropertiesPath :: FilePath -> IO FilePath
getPropertiesPath filename = do userDirectory <- getHomeDirectory
                                existFilenameOnUser <- doesFileExist $ combine userDirectory filename
                                return (if existFilenameOnUser then (combine userDirectory filename) else filename)


isAuditTrash '\160' = True
isAuditTrash '\NUL' = True
isAuditTrash '\9632' = True
isAuditTrash _ = False

workflow props input = show
                      $ map length
                      $ filter (\line -> length line > 0)
                      $ lines                                           -- cria listas com as linhas
                      $ filter (not . isAuditTrash) input               -- remove os carateres com lixo                      

debugProps = []           

main :: IO ()
main = do putStrLn version
          propertiesPath <- getPropertiesPath "Egna-ReadAudit.prop"
          putStrLn ("Read " ++ propertiesPath)
          propsRaw <- readFile propertiesPath
          props <- return $ debugProps ++ getProperties propsRaw
          mapM print props
          putStrLn ("Read " ++ getValue props "input.filename" "DUMP.log")
          input <- readFile $ getValue props "input.filename" "DUMP.log"
          output <- return $ workflow props input
          print output
          return ()

!