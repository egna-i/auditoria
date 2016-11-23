>module Main where
>import System.Directory (doesFileExist, getHomeDirectory)
>import System.FilePath.Windows (combine)
>import System.Time (Day(..),TimeDiff(..),CalendarTime(..),Month(..), 
>                    toClockTime,addToClockTime,toUTCTime,getClockTime, diffClockTimes)

>version :: String
>version = "Egna-ReadAudit " ++ "version 2.0.0.1"


>type Key = String
>type Value = String
>type Property = (Key, Value)
>getProperties = map (\x -> (read x) :: Property) . lines

>getValue :: [Property] -> Key -> Value -> Value
>getValue p k v = maybe v id (lookup k p)
>conditional p k f l = if read $ getValue p k "True" then f l else l

>cellSeparator p = getValue p "output.table.cell.separator" "; "
>lineSeparator p = getValue p "output.table.line.separator" "\n"
>audit_global_header p = getValue p "input.section.header" "     MEI - CF7000     "

>getPropertiesPath :: FilePath -> IO FilePath
>getPropertiesPath filename = do userDirectory <- getHomeDirectory
>                                existFilenameOnUser <- doesFileExist $ combine userDirectory filename
>                                return (if existFilenameOnUser then (combine userDirectory filename) else filename)

>isAuditDateSection = (==58) . length . filter (=='*')
>isAuditSection = (==24) . length . filter (=='=') 
>isAuditSubSection = (==24) . length . filter (=='-') 
>isAuditHeader p xss = audit_global_header p == head (head xss)

>isAuditTrash '\160' = True
>isAuditTrash '\NUL' = True
>isAuditTrash '\9632' = True
>isAuditTrash _ = False

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

>get_number :: (Num a, Read a) => String -> a
>get_number = read . last . words
>get_string = last . words
>get_tempo_ligacao props day = let y = read $ getValue props "date.initial.year" "2014"
>                                  m = read $ getValue props "date.initial.month" "July"
>                                  d = read $ getValue props "date.initial.day" "03"
>                                  ct = toClockTime $ CalendarTime y m d 0 0 0 0 Sunday 0 "" 0 True
>                                  res = addToClockTime (TimeDiff 0 0 day 0 0 0 0) ct 
>                              in (\x -> (show $ ctYear x) ++ "-" ++ (show $ get_month_number $ ctMonth x) ++ "-" ++ (show $ ctDay x)) $ toUTCTime res


group by N => [ date * [[[a]]]]
 1 - **************
 2 - =================
 3 - ---------------
 4 - values


=================================================================================================================================
 
 
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
                  
>table_audit_header_cash a b c = a ++ "-" ++ b ++ " " ++ c
>table_audit_headers n = ["IMPRESSAO NUMERO", "VENDAS TOTAL", "NUMERO DE VENDAS", "NO INTERR. DE AL.", "TEMPO LIGACAO", "IMPRESSAO ANTERIOR"]
>                        ++ ( [table_audit_header_cash m p o | m <- map show [1..n], o <- ["CASH","CARTAO"], p <- ["QUANTIDADE","PRECO","VALOR"]])


>isTrash :: Char -> Bool
>isTrash '\160' = True
>isTrash '\NUL' = True
>isTrash '\9632' = True
>isTrash _ = False


>data Money = Euro Integer deriving (Eq, Show)
>data SeparatorType = Stars | Equals | Minus deriving (Eq, Show)

>data EgnaAuditSt1 = St1 { st_items :: [EgnaAuditItem1]} deriving (Eq, Show)
>data EgnaAuditItem1 = Topic1 String | NameValue1 String String | Item1 Integer Integer Money Money | Separator1 SeparatorType deriving (Eq, Show)

>data EgnaAudit = Audit [String] [EgnaAuditSection] deriving (Eq, Show)
>data EgnaAuditSection = Section String [EgnaAuditSectionItem] deriving(Eq, Show)
>data EgnaAuditSectionItem = NameValue String String | Item Integer Integer Money Money deriving (Eq, Show)

>isSeparatorStars = (==58) . length . filter (=='*')
>isSeparatorEquals = (==24) . length . filter (=='=') 
>isSeparatorMinus = (==24) . length . filter (=='-') 

>isSeparatorStarsN :: [String] -> Int -> Bool
>isSeparatorStarsN list n = let newlist = drop n list
>                           in if length newlist > 0 then isSeparatorStars $ head newlist else False


>isValue :: String -> Bool
>isValue str = all (\ch -> ch `elem` "1234567890.-/:" ) str
>isNumber :: String -> Bool
>isNumber str = all (\ch -> ch `elem` "1234567890.-" ) str


>getEgnaAuditSectionItem :: String -> Maybe EgnaAuditSectionItem
>getEgnaAuditSectionItem str | (length $ words str) == 4 && all isNumber (words str) = 
>                              let (a:b:c:d:_) = words str
>                                  toMoney = round . (100*) . id
>                              in Just $ Item (read a) (read b) (Euro $ toMoney $ read c) (Euro $ toMoney $ read d) 
>                            | (length $ words str) > 0 && (isValue $ last $ words str) = 
>                              let v = last $ words str
>                              in Just $ NameValue (unwords $ take ((\x -> x - 1) $ length $ words str) (words str)) v
>                            | otherwise = Nothing



readAudit :: [String] -> EgnaAuditSt -> EgnaAuditSt
readAudit [] state = state
readAudit (x:xs) state | isSeparatorStars x && (length xs > 0) && (isSeparatorStars $ head xs) = St ([Separator Stars, Topic (drop 58 $ reverse x), Separator Stars] ++ (st_items $ readAudit xs state))

                        | isSeparatorStars x && (length xs > 1) && (not $ isSeparatorStars $ head xs) && 
                                                ()
                        
isSeparatorStars x && length x > 58 = St ([Topic (drop 58 $ reverse x), Separator Stars] ++ (st_items $ readAudit xs state))
                       | isSeparatorStars x && length x == 58 = St (Separator Stars : (st_items $ readAudit xs state))
                       | 

                       | otherwise = readAudit xs state


