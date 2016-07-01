module Catalogue
( Catalogue          -- Tipo sinônimo de lista de livros
, Order(..)          -- Tipo auxiliar que indica uma ordem
, strCatalogue       -- Converte o catálogo em uma string
, readCatalogue      -- Lê o catálogo de um conjunto de blocos
, sort               -- Ordena o catálogo, segundo um comparador e uma ordem
, findBook           -- Recebe o código de um livro e retorna o seu índice no catálogo
, addBook            -- Adiciona um livro ao catálogo
, removeBook         -- Recebe um índice e remove o livro com esse índice
) where

import Book

-- Um catálogo é simplesmente uma lista de livros
type Catalogue = [Book]

type Comparator = (Book -> Book -> Ordering)

data Order = Crescent | Decrescent

-- Tipo de dados que indica como o catálogo deve ser ordenado
data Rule = Rule 
    { cmp :: Comparator
    , ord :: Order
    }

-- Converte o catálogo em uma string
strCatalogue :: Catalogue -> String    
strCatalogue [] = ""
strCatalogue (b:bs) = strBook b ++ strCatalogue bs

-- Lê o catálogo de uma lista de blocos de string
readCatalogue :: [[String]] -> Catalogue
readCatalogue [] = []
readCatalogue (b:bs) = (readBook b):(readCatalogue bs)

sort :: Catalogue -> Comparator -> Order -> Catalogue
sort books cmp ord = mergesort books (Rule cmp ord)

-- mergesort e merge são funções auxiliares para a ordenação dos livros

mergesort :: [Book] -> Rule -> [Book]
mergesort [] _ = []
mergesort [x] _ = [x]
mergesort xs rule = merge leftOrdered rightOrdered rule
    where half = div (length xs) 2
          leftOrdered = mergesort (take half xs) rule
          rightOrdered = mergesort (drop half xs) rule

merge :: [Book] -> [Book] -> Rule -> [Book]
merge xs [] _ = xs
merge [] ys _ = ys
merge (x:xs) (y:ys) rule =
    if anterior x y rule
    then x:(merge xs (y:ys) rule)
    else y:(merge (x:xs) ys rule)
    where
        anterior a b (Rule comparator Crescent) = comparator a b == LT
        anterior a b (Rule comparator Decrescent) = comparator a b == GT

-- Recebe um catálogo e um código, e retorna o índice no catálogo do livro que possui tal código
findBook :: Catalogue -> Int -> Int
findBook books wantedCode = auxSearch books wantedCode 0
    where auxSearch [] _ _ = error "Livro não encontrado!"
          auxSearch (b:bs) wantedCode index = if code b == wantedCode
                                              then index
                                              else auxSearch bs wantedCode (index+1)

-- Adiciona um livro ao catálogo
addBook books book = book:books

-- Remove o livro indicado por index do catálogo
removeBook books index = (take index books) ++ (drop (index+1) books)

