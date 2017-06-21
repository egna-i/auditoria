>module Egna.Table where

>import Egna.Audit
>import Data.Time.Calendar


>data Row = Row {
>       row_columns :: [String],
>       row_line :: [String]
>      } deriving (Eq, Show)

>data Table = Table {
>       table_header :: [String],
>       table_lines :: [[String]]
>      } deriving (Eq, Show)

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
>                                  m = read $ getValue props "date.initial.month" "07"
>                                  d = read $ getValue props "date.initial.day" "03"
>                                  ct = fromGregorian y m d
>                                  res = addDays day ct 
>                              in showGregorian res
