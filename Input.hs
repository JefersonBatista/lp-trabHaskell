module Input
( linesFromFile
, blocksFromLines
, blocksFromFile
) where

import Data.List
import System.IO

-- Tipo auxiliar na leitura dos blocos a partir das linhas
data ReadingStatus = InBlock | OutBlock

-- Retorna as linhas do arquivo indicado por path
linesFromFile :: FilePath -> IO [String]
linesFromFile path = do
   contents <- readFile path
   let fileLines = lines contents
   return fileLines

-- Divide uma lista de linhas em blocos
blocksFromLines :: [String] -> [[String]]
blocksFromLines [] = []
blocksFromLines lines = auxFunction lines [] OutBlock
    where auxFunction :: [String] -> [[String]] -> ReadingStatus -> [[String]]
          auxFunction [] blocks _ = blocks
          -- Caso em que não se está em um bloco
          auxFunction (l:ls) blocks OutBlock = {- Se a linha atual for vazia, segue sem fazer nada.
                                                  Caso contrário, inicia um bloco com a linha atual. -}
                                               if null l
                                               then auxFunction ls blocks OutBlock
                                               else auxFunction ls (createBlock l blocks) InBlock
          -- Caso em que se está em um bloco                                     
          auxFunction (l:ls) blocks InBlock  = {- Se a linha atual for vazia, só troca o estado.
                                                  Caso contrário, adiciona a linha atual ao último bloco. -}
                                               if null l
                                               then auxFunction ls blocks OutBlock
                                               else auxFunction ls (addToLastBlock l blocks) InBlock
          createBlock line blocks = blocks ++ [[line]]
          addToLastBlock line blocks = (init blocks) ++ [(last blocks) ++ [line]]

-- Retorna os blocos de linhas de um arquivo
blocksFromFile :: FilePath -> IO [[String]]
blocksFromFile path = do
    fileLines <- linesFromFile path
    let blocks = blocksFromLines fileLines
    return blocks

