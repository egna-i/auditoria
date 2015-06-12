\begin{code}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

\end{code}

\begin{code}
module Egna.Parsec where

import System.Time

import Egna.Audit
--import Text.Parsec

\end{code}

\begin{code}

--import Data.Char
import Numeric
import Control.Applicative
import Control.Monad (MonadPlus(..), ap)

import Text.Parsec hiding (many, optional, (<|>))
---- Hide a few names that are provided by Applicative.
--import Text.ParserCombinators.Parsec --hiding (many, optional, (<|>))
--import Text.Parsec.Pos
--import Text.Parsec.Prim
--import Text.Parsec.Char

{-
-- The Applicative instance for every Monad looks like this.
instance Applicative (GenParser s a) where
    pure  = return
    (<*>) = ap
	

-- The Alternative instance for every MonadPlus looks like this.
instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus
	-}
	

\end{code}

AuditTermite * [AuditMiele]
AuditMiele = AuditMieleItem (AuditMieleCash | AuditMieleCard)

begin{code}

csvFile = endBy line eol
line = sepBy cell (char ',' <|> char ';')
cell = many (noneOf ",\n" <|> noneOf ";\n")
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

end{code}


\begin{code}

type AuditTermiteDateTime = (Int, Int, Int)
data AuditTermiteDate = AuditTermiteDate System.Time.Day System.Time.Month Int AuditTermiteDateTime Int deriving (Eq, Show)

eol = char '\n' <|> char '\r'

dayofweek =   (try $ Sunday <$ string "Sun")
          <|> (try $ Monday <$  string "Mon")
		  <|> (try $ Tuesday <$ string "Tue")
		  <|> (try $ Wednesday <$ string "Wed")
		  <|> (try $ Thursday <$ string "Thu")
		  <|> (try $ Friday <$ string "Fri")
		  <|> (try $ Saturday <$ string "Sat")
		  <?> "Day of Week {Sun,Mon,Tue,Wed,Thu,Fri,Sat}"
		  
month =       (try $ January <$ string "Jan")
          <|> (try $ February <$  string "Feb")
		  <|> (try $ March <$ string "Mar")
		  <|> (try $ April <$ string "Apr")
		  <|> (try $ May <$ string "May")
		  <|> (try $ June <$ string "Jun")
		  <|> (try $ July <$ string "Jul")
		  <|> (try $ August <$ string "Aug")
		  <|> (try $ September <$ string "Sep")
		  <|> (try $ October <$ string "Oct")
		  <|> (try $ November <$ string "Nov")
		  <|> (try $ December <$ string "Dec")
		  <?> "Month {Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec}"
		     
auditTermiteDate = do
					spaces *> many (char '*') *> try eol *> string "Termite log, started at" *> char ' ' 
					dw <- dayofweek
					char ' '
					m <- month
					char ' '
					d <- count 2 (char ' ' <|> digit)
					char ' '
					hh <- count 2 digit
					char ':'
					mm <- count 2 digit
					char ':'
					ss <- count 2 digit
					char ' '
					y <- count 4 digit
					try eol *> many (char '*') *> try eol
					return $ AuditTermiteDate dw m (read d) (read hh, read mm, read ss) (read y)


		
test = readFile "DUMP_002.log" >>= return . parse (many auditTermiteDate) "falhou"
	   
	   
\end{code}

