module Update
( Update            -- Tipo sinônimo de lista de mudanças
, Change(..)        -- Construtor de mudança em catálogo
, update
, readUpdate
) where

import Book
import Catalogue

-- Uma atualização é simplesmente uma lista de mudanças
type Update = [Change]

-- Uma mudança pode ser de três tipos: alteração, inclusão ou exclusão
data Change = Alteration Book | Inclusion Book | Exclusion Int

-- Função que aplica uma lista de mudanças a um catálogo
update :: Catalogue -> Update -> Catalogue
update catalogue [] = catalogue
update catalogue (c:cs) = update (change catalogue c) cs

-- Função que aplica uma mudança a um catálogo
change :: Catalogue -> Change -> Catalogue
change catalogue (Alteration book) = addBook (removeBook catalogue index) book
    where index = findBook catalogue (code book)
change catalogue (Inclusion book) = addBook catalogue book
change catalogue (Exclusion wantedCode) = removeBook catalogue index
    where index = findBook catalogue wantedCode

-- Lê uma mudança em catálogo de um bloco de linhas
readChange :: [String] -> Change
readChange block | option == "a" = Alteration $ readBook bookInfo
                 | option == "i" = Inclusion  $ readBook bookInfo
                 | option == "e" = Exclusion  $ read (head bookInfo)
                 | otherwise = error "Opção inválida de atualização!"
                 where bookInfo = tail block
                       option = [ch | ch <- head block, ch /= ' ']

-- Lê uma atualização de uma lista de blocos de linhas
readUpdate :: [[String]] -> Update
readUpdate [] = []
readUpdate (b:bs) = (readChange b):(readUpdate bs)

