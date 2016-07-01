import System.Directory
import System.IO

import Book
import Catalogue
import Input
import Update

main = do
   catalogo <- blocksFromFile "catalogo.txt"
   atualizacao <- blocksFromFile "atual.txt"
   
   let library = readCatalogue catalogo
       changes = readUpdate atualizacao
       updatedLibrary = update library changes
       catalogoAtualizado = strCatalogue (sort updatedLibrary cmpCode Crescent)
   
   (tempName, tempHandle) <- openTempFile "." "temp"
   hPutStr tempHandle $ catalogoAtualizado
   hClose tempHandle
   
   saida <- openFile "saida.txt" WriteMode
   
   hPutStr saida "Lista de Livros Ordenada Crescentemente por Código:\n\n"
   hPutStr saida catalogoAtualizado
   
   hPutStr saida "Lista de Livros Ordenada Decrescentemente por Título:\n\n"
   hPutStr saida $ strCatalogue (sort updatedLibrary cmpTitle Decrescent)
   
   hPutStr saida "Lista de Livros Ordenada Crescentemente por Autor:\n\n"
   hPutStr saida $ strCatalogue (sort updatedLibrary cmpAuthor Crescent)
   
   hPutStr saida "Lista de Livros Ordenada Decrescentemente por Data de Publicação:\n\n"
   hPutStr saida $ strCatalogue (sort updatedLibrary cmpDate Decrescent)

   hClose saida
   
   removeFile "catalogo.txt"
   renameFile tempName "catalogo.txt"
   
