>module Main where
>import Data.List (intersperse, elemIndex,sort,groupBy,sortBy,(\\))
>import System.Directory (doesFileExist, getHomeDirectory)
>import System.FilePath.Windows (combine)
>import System.Time (Day(..),TimeDiff(..),CalendarTime(..),Month(..), 
>                    toClockTime,addToClockTime,toUTCTime,getClockTime, diffClockTimes)

>import Text.Parsec

>version = "Egna-ReadAudit " ++ "version 1.0.8.0"

>type Number = Int
>data Money = Money {money :: Float} deriving (Eq, Ord)

>instance Show Money where 
>   show (Money m) = show m

>instance Num Money where
>  (Money a) + (Money b) = Money (a + b)
>  (Money a) * (Money b) = Money (a * b)
>  abs = Money . abs . money
>  signum = Money . signum . money
>  fromInteger = Money . fromInteger

>data Audit = Audit { 
>	audit_global :: AuditGlobalSection, 
>	audit_install :: AuditInstallSection, 
>	audit_partial :: AuditPartialSection, 
>	audit_cash :: AuditCashSection,
>	audit_card :: AuditCardSection
>	} deriving (Eq, Show)
>data AuditGlobalSection = AuditGlobalSection { 
>       audit_global_impressao_numero :: Number, 
>       audit_global_valor_vendas :: Money,
>       audit_global_numero_vendas :: Number, 
>       audit_global_dinheiro_tubos :: Money,
>       audit_global_interrupcao :: Number,
>       audit_global_data :: Int
>      } deriving (Eq, Show)
>data AuditInstallSection = AuditInstallSection {
>       audit_install_impressao_numero :: Number,
>       audit_install_vendas :: Money,
>       audit_install_numero_vendas :: Number
>      } deriving (Eq, Show)
>data AuditPartialSection = AuditPartialSection {
>       audit_partial_desde_impressao_numero :: Number,
>       audit_partial_dinheiro_cofre :: Money
>      } deriving (Eq, Show)
>data AuditCashSection = AuditCashSection {
>       audit_cash_items :: [AuditItem]
>      } deriving (Eq, Show)
>data AuditCardSection = AuditCardSection {
>       audit_card_items :: [AuditItem]
>      } deriving (Eq, Show)
>data AuditItem = AuditItem {
>       audit_item_machine :: Number,
>       audit_item_value :: Number,
>       audit_item_price :: Money,
>       audit_item_value_price :: Money
>      } deriving (Eq, Show)

>data Row = Row {
>       row_columns :: [String],
>       row_line :: [String]
>      } deriving (Eq, Show)
>data Table = Table {
>       table_header :: [String],
>       table_lines :: [[String]]
>      } deriving (Eq, Show)

>type Property = (String, String)

>instance Ord Audit where
>  compare a1 a2 = compare (audit_global_impressao_numero $ audit_global a1) (audit_global_impressao_numero $ audit_global a2)

>get_month_number January   = 01
>get_month_number February  = 02
>get_month_number March     = 03
>get_month_number April     = 04
>get_month_number May       = 05
>get_month_number June      = 06
>get_month_number July      = 07
>get_month_number August    = 08
>get_month_number September = 09
>get_month_number October   = 10
>get_month_number November  = 11
>get_month_number December  = 12

>get_number = read . last . words
>get_money = Money . read . last . words
>get_string = last . words
>get_data =  read . (\(fst, _) -> fst) . (\w -> splitAt (maybe 0 id (elemIndex '-' w)) w) . last . words

>showTable p (Table h rs) = (concat $ intersperse (cellSeparator p) $ map show h) ++ (lineSeparator p)
>                        ++ (concat $ intersperse (lineSeparator p) $ map (\r -> concat $ intersperse (cellSeparator p) r) rs)

>getProperties = map (\x -> (read x) :: Property) . lines
>getValue p k v = maybe v id (lookup k p)

>conditional p k f l = if read $ getValue p k "True" then f l else l

>cellSeparator p = getValue p "output.table.cell.separator" "; "
>lineSeparator p = getValue p "output.table.line.separator" "\n"

>audit_global_header p = getValue p "input.section.header" "     MEI - CF7000     "
>isAuditDateSection s = (take 58 $ repeat '*') == (reverse $ take 58 s) 
>isAuditSection s = (take 24 $ repeat '=') == (reverse $ take 24 s)
>isAuditSubSection s = (take 24 $ repeat '-') == (reverse $ take 24 s)
>isAuditHeader p xss = audit_global_header p == head (head xss)
>isAuditTrash '\160' = True
>isAuditTrash '\NUL' = True
>isAuditTrash '\9632' = True
>isAuditTrash _ = False

> -- audits   :: (String -> Bool) -> [String] -> [[String]]
>audits f s =  case dropWhile f s of
>                [] -> []
>                s' -> w : audits f s''
>                      where (w, s'') = break f s'

>audits2 f s =  case dropWhile f s of
>                [] -> []
>                s' -> [z : w : []] ++ (audits2 f s'')
>                      where (w, s'') = break f s'
>                            z = takeWhile f s


>create_audit_cash_item = (\(a:b:c:d:_) -> AuditItem (read a) (read b) (Money $ read c) (Money $ read d)) . words
>create_audit_global (audit_header:_:_:_:_:a:b:c:d:e:f:_) _ =  
>                     AuditGlobalSection (get_number a) (get_money b) (get_number c) (get_money d) (get_number e) (get_data f)
>create_audit_global [] n = AuditGlobalSection n (Money 0) 0 (Money 0) 0 0
>create_audit_install (_:_:_:a:b:c:_) = AuditInstallSection (get_number a) (get_money b) (get_number c)
>create_audit_install [] = AuditInstallSection 0 (Money 0) 0
>create_audit_partial (_:a:_:b:_) = AuditPartialSection (get_number a) (get_money b)
>create_audit_partial [] = AuditPartialSection 0 (Money 0) 
>create_audit_cash ("PARCIAL VENDAS GRATUITAS":_:_:_:_:_:xs) = AuditCashSection (map create_audit_cash_item xs)
>create_audit_cash (_:_:_:xs) = AuditCashSection (map create_audit_cash_item xs)
>create_audit_cash [] = AuditCashSection []
>create_audit_card (_:_:_:xs) = AuditCardSection (map create_audit_cash_item xs)
>create_audit_card [] = AuditCardSection []
>create_audit n (a:b:c:_:[]) = Audit (create_audit_global a n) (create_audit_install b) (create_audit_partial c) (create_audit_cash []) (create_audit_card [])
>create_audit n (a:b:c:d:_:[]) = Audit (create_audit_global a n) (create_audit_install b) (create_audit_partial c) (create_audit_cash d) (create_audit_card [])
>create_audit n (a:b:c:d:e:_:[]) = Audit (create_audit_global a n) (create_audit_install b) (create_audit_partial c) (create_audit_cash e) (create_audit_card d)
>create_audit n _ = Audit (create_audit_global [] n) (create_audit_install []) (create_audit_partial []) (create_audit_cash []) (create_audit_card [])

>table_audit_header_cash a b c = a ++ "-" ++ b ++ " " ++ c
>table_audit_headers n = ["IMPRESSAO NUMERO", "VENDAS TOTAL", "NUMERO DE VENDAS", "NO INTERR. DE AL.", "TEMPO LIGACAO", "IMPRESSAO ANTERIOR"]
>                        ++ ( [table_audit_header_cash m p o | m <- map show [1..n], o <- ["CASH","CARTAO"], p <- ["QUANTIDADE","PRECO","VALOR"]])

>table_audit_item a = let xs = audit_cash_items $ audit_cash a
>                         ys = audit_card_items $ audit_card a
>                     in [(x, y) | x <- xs, y <- ys, audit_item_machine x == audit_item_machine y]

>table_audit p a = Row (table_audit_headers (read (getValue p "input.machines" "6") :: Number))
>                      ([ show $ audit_global_impressao_numero $ audit_global a
>                      , show $ audit_global_valor_vendas $ audit_global a
>                      , show $ audit_global_numero_vendas $ audit_global a
>                      , show $ audit_global_interrupcao $ audit_global a
>                      , show $ get_tempo_ligacao p (audit_global_data $ audit_global a)
>                      , show $ audit_partial_desde_impressao_numero $ audit_partial a
>                      ] ++ (concat $ 
>                       map (\(x, y) -> [ show $ audit_item_value x,
>                                         show $ audit_item_price x, 
>                                         show $ audit_item_value_price x,
>                                         show $ audit_item_value y, 
>                                         show $ audit_item_price y, 
>                                         show $ audit_item_value_price y
>                                       ]) $ 
>                       filter (\(x, _) -> audit_item_machine x <= (read (getValue p "input.machines" "6") :: Number)) $ 
>                       filter (\(x, y) -> audit_item_machine x > 0 && audit_item_machine y > 0) $ 
>                       table_audit_item a))
>

>getTableFromRows [] = Table [] []
>getTableFromRows r = Table (row_columns $ head r) (map row_line r)

>createMissingAudit l = let x1 = map (audit_global_impressao_numero . audit_global) l
>                       in l ++ [ create_audit x3 [] | x3 <- [1 .. (maximum x1)] \\ x1]
>createMissingItem n = AuditItem n 0 (Money 0) (Money 0)

>filterDuplicates l =  map (last . sort)
>                    $ groupBy (\a b -> (audit_partial_desde_impressao_numero $ audit_partial a) > 0 &&
>                                       (audit_partial_desde_impressao_numero $ audit_partial a) == 
>                                       (audit_partial_desde_impressao_numero $ audit_partial b)) l

>isSameItem a b = audit_item_machine a == audit_item_machine b --  audit_item_price a == audit_item_price b
>isSameItemOrd a b | audit_item_machine a == audit_item_machine b = EQ
>                  | audit_item_machine a > audit_item_machine b = GT
>                  | audit_item_machine a < audit_item_machine b = LT
>                  | otherwise = EQ

>sumAuditItems [] = createMissingItem 0
>sumAuditItems ((AuditItem a2 b2 c2 d2):[]) = (AuditItem a2 b2 c2 d2)
>sumAuditItems ((AuditItem a2 b2 c2 d2):x2) = let x1 = sumAuditItems x2
>                                             in AuditItem a2 (audit_item_value x1 + b2) c2 (audit_item_value_price x1 + d2)

>foldrAuditItem (AuditItem a1 b1 c1 d1) (AuditItem a2 b2 c2 d2) = AuditItem (a1) (b1 + b2) (c1) (d1 + d2)

>foldrAuditFunc a b = Audit (AuditGlobalSection (max (audit_global_impressao_numero $ audit_global a) 
>                                                    (audit_global_impressao_numero $ audit_global b))
>                                               (max (audit_global_valor_vendas $ audit_global a) 
>                                                    (audit_global_valor_vendas $ audit_global b))
>                                               (max (audit_global_numero_vendas $ audit_global a) 
>                                                    (audit_global_numero_vendas $ audit_global b))
>                                               (max (audit_global_dinheiro_tubos $ audit_global a) 
>                                                    (audit_global_dinheiro_tubos $ audit_global b))
>                                               ((audit_global_interrupcao $ audit_global a) + 
>                                                (audit_global_interrupcao $ audit_global b))
>                                               (max (audit_global_data $ audit_global a) 
>                                                    (audit_global_data $ audit_global b)) )
>                           (AuditInstallSection (max (audit_install_impressao_numero $ audit_install a) 
>                                                     (audit_install_impressao_numero $ audit_install b))
>                                                (max (audit_install_vendas $ audit_install a) 
>                                                     (audit_install_vendas $ audit_install b))
>                                                (max (audit_install_numero_vendas $ audit_install a) 
>                                                     (audit_install_numero_vendas $ audit_install b))) 
>                           (AuditPartialSection (max (audit_partial_desde_impressao_numero $ audit_partial a) 
>                                                     (audit_partial_desde_impressao_numero $ audit_partial b))
>                                                ((audit_partial_dinheiro_cofre $ audit_partial a) + 
>                                                 (audit_partial_dinheiro_cofre $ audit_partial b))) 
>                           (AuditCashSection $ (map $ foldr foldrAuditItem (createMissingItem 0)) 
>                                             $ groupBy isSameItem 
>                                             $ sortBy isSameItemOrd 
>                                             $ ((audit_cash_items $ audit_cash a) ++ (audit_cash_items $ audit_cash b) )) 
>                           (AuditCardSection $ (map $ foldr foldrAuditItem (createMissingItem 0))
>                                             $ groupBy isSameItem 
>                                             $ sortBy isSameItemOrd 
>                                             $ ((audit_card_items $ audit_card a) ++ (audit_card_items $ audit_card b) )) 



>foldrAudit l = foldr foldrAuditFunc (create_audit 0 []) l

>createGroupAudit p l = 
>                if read $ getValue p "workflow.filtergroup" "True" 
>                then (map foldrAudit
>                   $ groupBy (\a b -> (get_tempo_ligacao p $ audit_global_data $ audit_global a) == 
>                                      (get_tempo_ligacao p $ audit_global_data $ audit_global b)) l)
>                else l ++ (map foldrAudit
>                   $ groupBy (\a b -> (get_tempo_ligacao p $ audit_global_data $ audit_global a) == 
>                                      (get_tempo_ligacao p $ audit_global_data $ audit_global b)) l)

>workflow props input = showTable props 													-- criar CSV output
>                      $ getTableFromRows 													-- criar Table
>                      $ map (table_audit props) 											-- criar Rows
>                      $ conditional props "workflow.sort" sort                  			-- ordenar as Auditorias pelo numero
>                      $ filter ((>0) . audit_global_impressao_numero . audit_global) 		-- filtrar auditorias com o numero 0
>                      $ conditional props "workflow.creategroup" (createGroupAudit props)	-- eliminar audits 
>                      $ conditional props "workflow.filterduplicates" filterDuplicates		-- eliminar audits 
>                      $ conditional props "workflow.createmissing" createMissingAudit		-- criar Audit em falta
>                      $ map (create_audit 0)												-- criar Audit
>                      $ filter (isAuditHeader props) 										-- filtar as secções inválidas
>                      $ map (audits isAuditSubSection) 									-- criação de subsecções
>                      $ audits isAuditSection 												-- criação de secções (auditorias)
>                      $ lines 																-- cria listas com as linhas
>                      $ filter (not . isAuditTrash) input 									-- remove os carateres com lixo

>workflow2 props input = --showTable props 													-- criar CSV output
> --                      $ getTableFromRows 													-- criar Table
> --                      $ map (table_audit props) 											-- criar Rows
> --                      $ conditional props "workflow.sort" sort                  			-- ordenar as Auditorias pelo numero
> --                      $ filter ((>0) . audit_global_impressao_numero . audit_global) 		-- filtrar auditorias com o numero 0
> --                      $ conditional props "workflow.creategroup" (createGroupAudit props)	-- eliminar audits 
> --                      $ conditional props "workflow.filterduplicates" filterDuplicates		-- eliminar audits 
> --                      $ conditional props "workflow.createmissing" createMissingAudit		-- criar Audit em falta
> --                       foldr (\(elem, acc) -> elem:acc) []												-- criar Audit
> --                      $ filter (isAuditHeader props) 										-- filtar as secções inválidas
>                        map (audits isAuditSubSection) 									-- criação de subsecções
>                       $ audits (\x -> isAuditSection x || isAuditDateSection x) 												-- criação de secções (auditorias)
>                       $ lines 																-- cria listas com as linhas
>                      $ filter (not . isAuditTrash) input 									-- remove os carateres com lixo

>assimilar [] = []
>assimilar (x:[]) = x
>assimilar (x:y:xs) | length x == 1 && length y /= 1 = (head x:y) ++ assimilar xs
>                   | otherwise = assimilar (y:xs)


>get_tempo_ligacao props day = let y = read $ getValue props "date.initial.year" "2014"
>                                  m = read $ getValue props "date.initial.month" "July"
>                                  d = read $ getValue props "date.initial.day" "03"
>                                  ct = toClockTime $ CalendarTime y m d 0 0 0 0 Sunday 0 "" 0 True
>                                  res = addToClockTime (TimeDiff 0 0 day 0 0 0 0) ct 
>                              in (\x -> (show $ ctYear x) ++ "-" ++ (show $ get_month_number $ ctMonth x) ++ "-" ++ (show $ ctDay x)) $ toUTCTime res

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

>main2 :: IO ()
>main2 = do putStrLn version
>           beforeWorkFlowTime <- getClockTime
>           propertiesPath <- getPropertiesPath "Egna-ReadAudit.prop"
>           putStrLn ("Read " ++ propertiesPath)
>           propsRaw <- readFile propertiesPath
>           props <- return $ getProperties propsRaw
>           putStrLn ("Read " ++ getValue props "input.filename" "DUMP.log")
>           input <- readFile $ getValue props "input.filename" "DUMP.log"
>           output <- return $ workflow props input
>           putStrLn ("Write " ++ getValue props "output.filename" "DUMP.csv")
>           writeFile (getValue props "output.filename" "DUMP.csv") output
>           afterWorkFlowTime <- getClockTime
>           putStrLn ("Workflow " ++ (show $ tdSec $ diffClockTimes afterWorkFlowTime beforeWorkFlowTime) ++ " secs")
>           return ()
