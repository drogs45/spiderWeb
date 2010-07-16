import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Html.Generate(showattr)
import Data.Char
import Data.List
import System.Environment

getTitle :: Content a -> String
getTitle doc =
    contentToStringDefault 
        (multi (tag "title" /> txt $) doc)

parse :: String -> String -> String
parse content name = getTitle doc
  where
    parseResult = xmlParse name (stripUnicodeBOM content)
    doc = getContent parseResult
    getContent :: Document i -> Content i
    getContent (Document _ _ e _) = CElem e (contentElem e)
    
    {- | Some Unicode documents begin with a binary sequence;
       strip it off before processing. -}
    stripUnicodeBOM :: String -> String
    stripUnicodeBOM ('\xef':'\xbb':'\xbf':x) = x
    stripUnicodeBOM x = x

contentToStringDefault :: [Content a] -> [String]
contentToStringDefault [] = [] 
contentToStringDefault x = map contentToString x
  where 
    procContent x =
      verbatim $ keep /> txt $ CElem (unesc (fakeElem x)) (contenidoCont x)
    fakeElem :: Content i -> Element i
    fakeElem x = Elem "fake" [] [x]
    unesc :: Element i -> Element i
    unesc = xmlUnEscape stdXmlEscaper

contentToString :: [Content a] -> String
contentToString = 
  concatMap procContent
    where 
      procContent x = 
        verbatim $ keep /> txt $ CElem (unesc (fakeElem x)) (contenidoCont x)
      fakeElem :: Content i -> Element i
      fakeElem x = Elem "fake" [] [x]
      unesc :: Element i -> Element i
      unesc = xmlUnEscape stdXmlEscaper
          
channel :: CFilter a
channel = tag "rss" /> tag "channel"

contenidoCont :: Content i -> i
contenidoCont (CElem (_) c) = c

contentElem :: Element i -> i
contentElem (Elem name [attr] [j])  = contenidoCont j

main = do
  x <- getArgs
  content <- readFile $ head x
  print $ parse content "error"

