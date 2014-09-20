>module Main where
>import Data.List(intersperse)
>import System.Directory(doesFileExist, getHomeDirectory)
>import System.FilePath.Windows(combine)

>data Money = Money {money :: Double} --deriving (Eq)

>instance Show Money where
>  show (Money m) = show m

>data Audit = Audit { 
>	audit_global :: AuditGlobalSection, 
>	audit_install :: AuditInstallSection, 
>	audit_partial :: AuditPartialSection, 
>	audit_cash :: AuditCashSection,
>	audit_card :: AuditCardSection
>	} --deriving (Eq, Show)

>data AuditGlobalSection = AuditGlobalSection { 
>       audit_global_impressao_numero :: Integer, 
>       audit_global_valor_vendas :: Money,
>       audit_global_numero_vendas :: Integer, 
>       audit_global_dinheiro_tubos :: Money
>      } --deriving (Eq, Show)
>
>data AuditInstallSection = AuditInstallSection {
>       audit_install_impressao_numero :: Integer,
>       audit_install_vendas :: Money,
>       audit_install_numero_vendas :: Integer
>      } --deriving (Eq, Show)
>data AuditPartialSection = AuditPartialSection {
>       audit_partial_desde_impressao_numero :: Integer,
>       audit_partial_dinheiro_cofre :: Money
>      } --deriving (Eq, Show)
>data AuditCashSection = AuditCashSection {
>       audit_cash_items :: [AuditItem]
>      } --deriving (Eq, Show)
>data AuditCardSection = AuditCardSection {
>       audit_card_items :: [AuditItem]
>      } --deriving (Eq, Show)
>data AuditItem = AuditItem {
>       audit_item_machine :: Integer,
>       audit_item_value :: Integer,
>       audit_item_price :: Money,
>       audit_item_value_price :: Money
>      } --deriving (Eq, Show)

>data Row = Row {
>       row_columns :: [String],
>       row_line :: [String]
>      } --deriving (Eq, Show)

>data Table = Table {
>       table_header :: [String],
>       table_lines :: [[String]]
>      } --deriving (Eq, Show)

>showTable p (Table h rs) = (concat $ intersperse (cellSeparator p) $ map show h) ++ (lineSeparator p)
>                        ++ (concat $ intersperse (lineSeparator p) $ map (\r -> concat $ intersperse (cellSeparator p) r) rs)

>type Property = (String, String)

>cellSeparator p = getValue p "output.table.cell.separator" "; "
>lineSeparator p = getValue p "output.table.line.separator" "\n"

>audit_global_header p = getValue p "input.section.header" "     MEI - CF7000     "
>isAuditSection s = (take 24 $ repeat '=') == (reverse $ take 24 s)
>isAuditSubSection s = (take 24 $ repeat '-') == (reverse $ take 24 s)
>isAuditTrash '\160' = True
>isAuditTrash '\NUL' = True
>isAuditTrash '\9632' = True
>isAuditTrash _ = False
>isAuditHeader p xss = audit_global_header p == head (head xss)

>audits   :: (String -> Bool) -> [String] -> [[String]]
>audits f s =  case dropWhile f s of
>                [] -> []
>                s' -> w : audits f s''
>                      where (w, s'') = break f s'

>get_number = read . last . words
>get_money = Money . read . last . words
>create_audit_cash_item = (\(a:b:c:d:_) -> AuditItem (read a) (read b) (Money $ read c) (Money $ read d)) . words

>create_audit_global (audit_header:_:_:_:_:a:b:c:d:_) =  AuditGlobalSection (get_number a) (get_money b) (get_number c) (get_money d)
>create_audit_global [] = AuditGlobalSection 0 (Money 0) 0 (Money 0)
>create_audit_install (_:_:_:a:b:c:_) = AuditInstallSection (get_number a) (get_money b) (get_number c)
>create_audit_install [] = AuditInstallSection 0 (Money 0) 0
>create_audit_partial (_:a:_:b:_) = AuditPartialSection (get_number a) (get_money b)
>create_audit_partial [] = AuditPartialSection 0 (Money 0) 
>create_audit_cash ("PARCIAL VENDAS GRATUITAS":_:_:_:_:_:xs) = AuditCashSection (map create_audit_cash_item xs)
>create_audit_cash (_:_:_:xs) = AuditCashSection (map create_audit_cash_item xs)
>create_audit_cash [] = AuditCashSection []
>create_audit_card (_:_:_:xs) = AuditCardSection (map create_audit_cash_item xs)
>create_audit_card [] = AuditCardSection []
>create_audit (a:b:c:_:[]) = Audit (create_audit_global a) (create_audit_install b) (create_audit_partial c) (create_audit_cash []) (create_audit_card [])
>create_audit (a:b:c:d:_:[]) = Audit (create_audit_global a) (create_audit_install b) (create_audit_partial c) (create_audit_cash d) (create_audit_card [])
>create_audit (a:b:c:d:e:_:[]) = Audit (create_audit_global a) (create_audit_install b) (create_audit_partial c) (create_audit_cash e) (create_audit_card d)
>create_audit _ = Audit (create_audit_global []) (create_audit_install []) (create_audit_partial []) (create_audit_cash []) (create_audit_card [])

>table_audit_header_cash a b c = a ++ "-" ++ b ++ " " ++ c
>table_audit_headers n = ["IMPRESSAO NUMERO", "VENDAS TOTAL","NUMERO DE VENDAS","IMPRESSAO ANTERIOR"]
>                        ++ ( [table_audit_header_cash m p o | m <- map show [1..n], o <- ["CASH","CARTAO"], p <- ["QUANTIDADE","PRECO","VALOR"]])

>table_audit_item a = let xs = audit_cash_items $ audit_cash a
>                         ys = audit_card_items $ audit_card a
>                     in [(x, y) | x <- xs, y <- ys, audit_item_machine x == audit_item_machine y]

>table_audit p a = Row (table_audit_headers (read (getValue p "input.machines" "6") :: Integer))
>                      ([ show $ audit_global_impressao_numero $ audit_global a
>                      , show $ audit_global_valor_vendas $ audit_global a
>                      , show $ audit_global_numero_vendas $ audit_global a
>                      , show $ audit_partial_desde_impressao_numero $ audit_partial a
>                      ] ++ (concat $ 
>                       map (\(x, y) -> [ show $ audit_item_value x, 
>                                         show $ audit_item_price x, 
>                                         show $ audit_item_value_price x,
>                                         show $ audit_item_value y, 
>                                         show $ audit_item_price y, 
>                                         show $ audit_item_value_price y]) $ 
>                       filter (\(x, _) -> audit_item_machine x <= (read (getValue p "input.machines" "6") :: Integer)) $ table_audit_item a))
>

>getTableFromRows [] = Table [] []
>getTableFromRows r = Table (row_columns $ head r) (map row_line r)

>maudits p s = map (table_audit p) $
>              filter ((>0) . audit_global_impressao_numero . audit_global) $
>              map create_audit $
>              filter (isAuditHeader p) $ 
>              map (audits isAuditSubSection) $ 
>              (audits isAuditSection) $ 
>              lines (filter (not . isAuditTrash) s)

>getProperties = map (\x -> (read x) :: Property) . lines
>getValue p k v = maybe v id (lookup k p)

>getPropertiesPath filename = do existFilename <- doesFileExist filename
>                                userDirectory <- getHomeDirectory
>                                existFilenameOnUser <- doesFileExist $ combine userDirectory filename
>                                return (if existFilenameOnUser then (combine userDirectory filename) else filename)

>main :: IO ()
>main = do propertiesPath <- getPropertiesPath "Egna-ReadAudit.prop"
>          propsRaw <- readFile propertiesPath
>          props <- return $ getProperties propsRaw
>          input <- readFile $ getValue props "input.filename" "DUMP.log"
>          output <- return $ showTable props $ getTableFromRows $ maudits props input
>          writeFile (getValue props "output.filename" "DUMP.csv") output
>          return ()
