>module Egna.Table where
>import System.Time (Day(..),TimeDiff(..),CalendarTime(..),Month(..), 
>                    toClockTime,addToClockTime,toUTCTime,getClockTime, diffClockTimes)

>import Egna.Audit

>data Row = Row {
>       row_columns :: [String],
>       row_line :: [String]
>      } deriving (Eq, Show)

>data Table = Table {
>       table_header :: [String],
>       table_lines :: [[String]]
>      } deriving (Eq, Show)


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
>                      , show $ get_tempo_ligacao p (audit_day $ audit_global_data $ audit_global a)
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

>get_tempo_ligacao props day = let y = read $ getValue props "date.initial.year" "2014"
>                                  m = read $ getValue props "date.initial.month" "July"
>                                  d = read $ getValue props "date.initial.day" "03"
>                                  ct = toClockTime $ CalendarTime y m d 0 0 0 0 Sunday 0 "" 0 True
>                                  res = addToClockTime (TimeDiff 0 0 day 0 0 0 0) ct 
>                              in (\x -> (show $ ctYear x) ++ "-" ++ (show $ get_month_number $ ctMonth x) ++ "-" ++ (show $ ctDay x)) $ toUTCTime res

