>module Egna.Audit where
>import Data.List (elemIndex)

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
>       audit_global_data :: AuditDateTime
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

>data AuditDateTime = AuditDateTime {
>       audit_year :: Int,
>       audit_day :: Int,
>       audit_minute :: Int
>      } deriving (Eq, Show)

>instance Ord Audit where
>  compare a1 a2 = compare (audit_global_impressao_numero $ audit_global a1) (audit_global_impressao_numero $ audit_global a2)


>get_number = read . last . words
>get_money = Money . read . last . words
>get_string = last . words
>get_data =  read . (\(fst, _) -> fst) . (\w -> splitAt (maybe 0 id (elemIndex '-' w)) w) . last . words

>getValue p k v = maybe v id (lookup k p)
>audit_global_header p = getValue p "input.section.header" "     MEI - CF7000     "
>isAuditDateSection s = (take 58 $ repeat '*') == (reverse $ take 58 s) 
>isAuditSection s = (take 24 $ repeat '=') == (reverse $ take 24 s)
>isAuditSubSection s = (take 24 $ repeat '-') == (reverse $ take 24 s)
>isAuditHeader p xss = audit_global_header p == head (head xss)

>audits   :: (String -> Bool) -> [String] -> [[String]]
>audits f s =  case dropWhile f s of
>                [] -> []
>                s' -> w : audits f s''
>                      where (w, s'') = break f s'

>create_audit_global_data year day minute = AuditDateTime year day minute
>create_audit_cash_item = (\(a:b:c:d:_) -> AuditItem (read a) (read b) (Money $ read c) (Money $ read d)) . words
>create_audit_global (audit_header:_:_:_:_:a:b:c:d:e:f:_) _ =  
>                     AuditGlobalSection (get_number a) (get_money b) (get_number c) (get_money d) (get_number e) (create_audit_global_data 2014 (get_data f) 0)
>create_audit_global [] n = AuditGlobalSection n (Money 0) 0 (Money 0) 0 (create_audit_global_data 2000 1 0)
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

>parse_audit p n str | audit_global_header p == (head $ lines str) = 
>                            create_audit n
>                          $ audits isAuditSubSection 
>                          $ lines str
>parse_audit p n str | otherwise =  
>                            create_audit n
>                          $ audits isAuditSubSection 
>                          $ lines str
>


