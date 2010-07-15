{--module XMLParser
  ( 
          -- * Operaciones
        getText,                -- :: 
        getLinks,              -- :: 
        parseLinks,               -- :: 
        parseText,
        contentToStringDefault,
        contentToString,
        contenidoCont,
        contentElem

          -- * Stream interface
       -- getChanContents,        -- :: Chan a -> IO [a]
       -- writeList2Chan,         -- :: Chan a -> [a] -> IO ()
   ) where --}

import Prelude

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Either
import Network.HTTP as H
import Network.Browser
import Network.URI
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Html.Generate(showattr)
import System
import Data.Char
import Data.List( isInfixOf, isPrefixOf)

----------------
filtroGeneral :: String -> (String -> Bool) -> Bool
filtroGeneral file fun = fun file

filtroLinks :: Int -> [String] -> [String]
filtroLinks n links = take n links

variasPalabras:: [String] -> [String]-> Bool
variasPalabras words file = fold unaPalabra file words
  where
    fold :: ([String] -> String -> Bool) -> [String] -> [String]-> Bool
    fold _       _ [] = True
    fold funtion file (word:words) =
      case funtion file word of
        True  -> fold funtion file words 
        False -> False

xOcurrenciaPalabra :: Int -> String -> [String] -> Bool
xOcurrenciaPalabra n word file = n == (anyC word file 0)

almenosxOcurrenciaPalabra :: Int -> String -> [String] -> Bool
almenosxOcurrenciaPalabra n word file = n <= (anyC word file 0)

alosumoxOcurrenciaPalabra :: Int -> String -> [String] -> Bool
alosumoxOcurrenciaPalabra n word file = n >= (anyC word file 0)

anyC :: String -> [String] -> Int-> Int
anyC _ [] a = a
anyC word (file:files) a = 
  case (word `isInfixOf`) file of
    True  -> anyC word files su
    False -> anyC word files a
  where
    !su = a+1

unaPalabra :: [String] -> String -> Bool
unaPalabra file word = any (word `isInfixOf`) file
------------------

links :: String -> [String] -> [String]
links a [] = []
links a (x:xs) =
  case "http" `isPrefixOf` x of
    True  -> x :links a xs
    False -> (a++x) : links a xs

-- Dado el contenido de una página obtenido al parsearla se retorna una lista de
-- String con el texto plano, extraido de todos los tags.
getText :: Content a -> [String]
getText doc =
    contentToStringDefault "No texto" 
        (multi (txt $) doc)

-- Dado el contenido (Content a) de una página obtenido al parsearla se retorna 
-- una lista de String con los links encontrados.
getLinks :: Content a -> [String]
getLinks doc =
    --contentToStringDefault "Untitled Podcast" 
         map valor $ map (getatt "href") a
          where
          a = map getname $ (multi ( attr "href" `o` tag "a" $)) $ doc

-- Dado el contenido de una busqueda a través de un filtro, se obtiene la lista
-- de atributos de ese tag.
getname :: Content a -> [Attribute]
getname (CElem (Elem tg atributos _) _) = atributos

-- Dado un String con el nombre del atributo del cual queremos obtener el valor
-- y la lista de atributos de dicho tag retorno un AttValue.
getatt :: String -> [(Name,AttValue)] -> AttValue 
getatt natr lista = snd $ head $ filter (igual natr) lista
                   where igual n (a,b) = n == a

-- Dado un AttValue que es un Either de un String o una Referencia, obtengo
-- el valor del atributo.
valor :: AttValue -> String
valor (AttValue (algo:_)) = case algo of
                             Left a -> a
                             Right b -> "error"

-- Dado el texto del HTML y un mensaje extra de error, retorno lista de links.
parseLinks :: String -> String -> String -> [String]
parseLinks linkp content name = links linkp $ getLinks doc

        where parseResult = htmlParse name (stripUnicodeBOM content)
              doc = getContent parseResult

              -- A partir de un Documento obtengo su contenido
              getContent :: Document i -> Content i
              getContent (Document _ _ e _) = CElem e (contentElem e)
          
          {- | Some Unicode documents begin with a binary sequence;
             strip it off before processing. -}
              stripUnicodeBOM :: String -> String
              stripUnicodeBOM ('\xef':'\xbb':'\xbf':x) = x
              stripUnicodeBOM x = x
              
parseText :: String -> String -> [String]
parseText content name = getText doc

        where parseResult = htmlParse name (stripUnicodeBOM content)
              doc = getContent parseResult

              getContent :: Document i -> Content i
              getContent (Document _ _ e _) = CElem e (contentElem e)
          
          {- | Some Unicode documents begin with a binary sequence;
             strip it off before processing. -}
              stripUnicodeBOM :: String -> String
              stripUnicodeBOM ('\xef':'\xbb':'\xbf':x) = x
              stripUnicodeBOM x = x
          
contentToStringDefault :: String -> [Content a] -> [String]
contentToStringDefault msg [] = []
contentToStringDefault _ x = contentToString x

-- A partir de una lista x de contenidos obtenida a partir de filtrar un
-- Content a, se crea un Elemento cuyo contenido es la lista de Content a.
contentToString :: [Content a] -> [String]
contentToString = 
    map procContent
    where procContent x = 
              -- se transforma a string el contenido txt obtenido.
              verbatim $ keep /> txt $ CElem (unesc (fakeElem x)) (contenidoCont x)

          -- A partir de un Contenido genera un Element que servira como
          -- envoltorio.
          fakeElem :: Content i -> Element i
          fakeElem x = Elem "fake" [] [x]

          unesc :: Element i -> Element i
          unesc = xmlUnEscape stdXmlEscaper

-- Funcion Auxiliar que retorna el contenido de Content.
contenidoCont :: Content i -> i
contenidoCont (CElem (_) c) = c

-- Funcion Auxiliar que retorna el contenido de un elemento.
contentElem :: Element i -> i
contentElem (Elem name [attr] [j])  = contenidoCont j

-- Transforma código de respuesta en string con error de salida
toerr :: ResponseCode -> String
toerr (a,b,c) = map intToDigit [a,b,c]

uritostring :: URI -> String
uritostring (URI a (Just (URIAuth t s u)) c d e) = a++t++s++u++c++d++e
uritostring (URI a Nothing c d e) = a++c++d++e

conti :: (URI,Response String) -> String
conti (a,Response b c d e) = e

-- Visita un link y retorna contenido del request.
visitarp :: String -> Int -> IO ()
visitar _ 0 = print "fin"
visitarp s r = conti rsp
               where
                rsp = Network.Browser.browse $ do
                 setAllowRedirects True -- handle HTTP redirects
                 request $ getRequest s
             {--case rsp of
              (a,Response b c d e) -> if "4" `isPrefixOf` (toerr b) || 
                                         "5" `isPrefixOf` (toerr b)
                                       then print $ "Error:" ++ (toerr b) ++ " " ++ c
                                       else map visitarp (parseLinks (uritostring a) e "Error de Parseo") --}
             
               
{-- Recibe Request y retorna contenido o error.
cont :: IO (URI,Response String) -> String
cont IO (a,Response b c d e) = if "4" `isPrefixOf` (toerr b) || 
                                   "5" `isPrefixOf` (toerr b)
                                 then "Error:" ++ (toerr b) ++ " " ++ c
                                 else e--}

main = do
       p <- getArgs -- argumento página raiz
       let s = p!!0
       rsp <- Network.Browser.browse $ do
               setAllowRedirects True -- handle HTTP redirects
               request $ getRequest s
       case rsp of
        (a,Response b c d e) -> if "4" `isPrefixOf` (toerr b) || 
                                   "5" `isPrefixOf` (toerr b)
                                 then print $ "Error:" ++ (toerr b) ++ " " ++ c
                                 else print e
       -- a <- fmap (drop 100) (getResponseBody (snd rsp))
       {--case rsp of
        Left (s) -> print s
        Right (Response b c d e) -> if (toerr b) == "200"
                                     then print e 
                                     else  print $ (toerr b) ++ " " ++ c
       -- print $ parseLinks "http://www.haskell.org/" x "error"
       --let a = parseText x "error"
       -- print $ a--}
