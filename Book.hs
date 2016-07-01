module Book
( Book(..)          -- Construtor de livro
, strBook           -- Converte o livro em uma string
, readBook          -- Lê um livro de um bloco de linhas
, cmpCode           -- Compara livros por código
, cmpTitle          -- Compara livros por título
, cmpAuthor         -- Compara livros por autor
, cmpDate           -- Compara livros por data
) where

import Data.Char
import Date

data Book = Book
    { code :: Int
    , title :: String
    , author :: String
    , guess :: String
    , date :: Date
    , publisher :: String
    , summary :: String
    }

strBook book = (show $ code book) ++ "\n" ++
               (title book) ++ "\n" ++
               (author book) ++ "\n" ++
               (guess book) ++ "\n" ++
               (strDate $ date book) ++ "\n" ++
               (publisher book) ++ "\n" ++
               (summary book) ++ "\n\n"

readBook :: [String] -> Book
readBook block = Book { code         = read $ block !! 0
                      , title               = block !! 1
                      , author              = block !! 2
                      , guess               = block !! 3
                      , date = stringToDate $ block !! 4
                      , publisher           = block !! 5
                      , summary             = getSummary $ drop 6 block
                      }
                      where getSummary [] = ""
                            getSummary [l] = l
                            getSummary (l:ls) = (l ++ "\n") ++ getSummary ls

cmpCode b1 b2 = compare (code b1) (code b2)

cmpTitle b1 b2 | comparison /= EQ = comparison
               | otherwise = cmpCode b1 b2
               where comparison = compare (lowercase $ title b1) (lowercase $ title b2)
    
cmpAuthor b1 b2 | comparison /= EQ = comparison
                | otherwise = cmpCode b1 b2
                where comparison = compare (lowercase $ author b1) (lowercase $ author b2)
    
cmpDate b1 b2 | comparison /= EQ = comparison
              | otherwise = cmpCode b1 b2
              where comparison = compareDate (date b1) (date b2)

-- Função auxiliar para deixar uma string só com letras minúsculas
lowercase str = map toLower str

