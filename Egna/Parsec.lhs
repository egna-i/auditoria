\begin{code}
module Egna.Parsec where

import Egna.Audit
import Text.Parsec
import Text.ParserCombinators.Parsec

\end{code}


\begin{code}

csvFile = endBy line eol
line = sepBy cell (char ',' <|> char ';')
cell = many (noneOf ",\n" <|> noneOf ";\n")
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

\end{code}


\begin{code}

starsOfDUMP = "**********************************************************"

parse_timing = do 
    many (char '*') >>
	eol >>
	string "Termite log, started at" >>
	return ()
	


--skipMany (string starsOfDUMP <|> eol) <|> 

\end{code}

