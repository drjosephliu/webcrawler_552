module Html where

import Prelude hiding (filter)
import Data.Char
import Data.List (intercalate, isPrefixOf)
import qualified Data.DList as DL

import Test.HUnit
import Test.QuickCheck

import Control.Applicative 

import Text.PrettyPrint(Doc)
import qualified Text.PrettyPrint as PP

import Parser ( Parser
              , doParse
              , get
              , satisfy
              , filter
              , alpha
              , digit
              , space
              , char
              , string
              , wsP
              , lookahead)


-- ============================================================================
-- | DATATYPES
-- ============================================================================

data SimpleHTML = PCDATA String
                | Element ElementName [TagAttribute] [SimpleHTML]
                | Comment String
                deriving Show

data TagAttribute = Attribute AttributeName AttributeValue 
                  deriving Show

type AttributeName = String
type AttributeValue = String
type ElementName = String

instance Eq TagAttribute where
  Attribute n1 v1 == Attribute n2 v2 = n1 == n2 && v1 == v2

instance Eq SimpleHTML where
  PCDATA s1        == PCDATA s2        = s1 == s2
  Comment s1       == Comment s2       = s1 == s2
  Element t1 a1 s1 == Element t2 a2 s2 = t1 == t2 && a1 == a2 && s1 == s2
  _                == _                = False

-- ============================================================================
-- | METHODS
-- ============================================================================

-- | List of tags that don't require closing tags
isSelfClosing :: String -> Bool
isSelfClosing s = s `elem` ["br", "img", "meta", "link", "area", "base", "br",
  "col", "command", "embed", "hr", "input", "keygen", "param", "source",
  "track", "wbr", "image", "path", "circle"]

-- | Parses HTML tag name
tagName :: Parser String
tagName = some (alpha <|> digit)

-- =====================================
-- | Tag attribute name helper functions
-- =====================================

-- | Special characters allowed in attribute names
isBridge :: Char -> Bool
isBridge c = c `elem` ['_', '-', ':']

-- | Parses boolean attributes, i.e. no value rquired
battrName :: Parser String
battrName = some (satisfy (\c -> (isAlpha c || isDigit c || isBridge c)))

-- | Parses regular attribute names, e.g. src=
rattrName :: Parser String
rattrName = battrName <* (wsP (char '='))

-- ======================================
-- | Tag attribute value helper functions
-- =====================================-

-- | Checks for valid chars in unquoted attribute values
isUnquoted :: Char -> Bool
isUnquoted c = not (c `elem` [' ', '\"', '\'', '<', '>'])

-- | Parses unquoted attribute values
unquotedP :: Parser String
unquotedP = some (satisfy isUnquoted)

-- | Parses attribute values that are surrounded by w, e.g. single/double quote
wrapP :: Char -> Parser String
wrapP w = char w *> (many (satisfy (\c -> c /= w))) <* char w

-- | Parses attribute value
attrVal :: Parser String
attrVal = wrapP '\"' <|> wrapP '\'' <|> unquotedP

-- ======================
-- | Tag attribute parser
-- ======================

-- | Parses both regular and boolean attributes. 
-- | For regular, it also parses both unquoted and quoted values
tagAttr :: Parser TagAttribute
tagAttr = wsP $ regAttr <|> boolAttr
  where regAttr  = Attribute <$> rattrName <*> (wsP attrVal)
        boolAttr = Attribute <$> battrName <*> pure []

-- =====================
-- | PCDATA text parser
-- =====================

text :: Parser String
text = some (satisfy (\c -> c /= '<'))

pcdata :: Parser SimpleHTML
pcdata = PCDATA <$> text

-- =====================
-- | HTML comment parser
-- =====================

comment :: Parser SimpleHTML
comment = Comment <$> wsP (string "<!--" *> (many (lookahead "-->")))
                  <*  wsP (string "-->")

-- =======================
-- | HTML container parser
-- =======================

-- | Parses only <script> containers
script :: Parser SimpleHTML
script = Element <$> wsP (char '<' *> (wsP (string "script")))
                 <*> wsP ((many tagAttr) <* (wsP (char '>')))
                 <*> pure []
                 <*  wsP (many (lookahead "</script>") <* string "</script>")

-- | Parses only <style> containers
style :: Parser SimpleHTML
style = Element <$> wsP (char '<' *> (wsP (string "style")))
                <*> wsP ((many tagAttr) <* (wsP (char '>')))
                <*> pure []
                <*  wsP (many (lookahead "</style>") <* string "</style>")

-- | Parses only <li> containers. ALlows for optional closing tag.
li :: Parser SimpleHTML
li = Element <$> wsP (char '<' *> (wsP (string "li")))
             <*> wsP ((many tagAttr) <* (wsP (char '>')))
             <*> many html
             <*  wsP (many (string "</li>"))

-- | Parses <ul> and <ol> containers
list :: Parser SimpleHTML
list = (\(w,x,y,z) -> Element w x y) <$> 
        filter (\(w,x,y,z) -> w == z && isTag w) quad
 where isTag t = t `elem` ["ol", "ul"]
       quad = (,,,) <$> wsP (char '<' *> (wsP tagName))
                    <*> wsP ((many tagAttr) <* char '>')
                    <*> many li
                    <*> wsP (string "</" *> tagName <* char '>')

-- | Parses empty containers
emptyContainer :: Parser SimpleHTML
emptyContainer = (\(x,y) -> Element x y []) <$> 
                     filter (\(x,y) -> isSelfClosing x) double 
  where double = (,) <$> wsP (char '<' *> (wsP tagName))
                     <*> wsP (wsP (many tagAttr) <* 
                                  (string ">" <|> string "/>"))

-- | Parses empty containers which have optional closing tag, e.g. <path></path>
semiEmptyContainer :: Parser SimpleHTML
semiEmptyContainer = (\(x,y,z) -> Element x y []) <$> 
                        filter (\(x,y,z) -> isSelfClosing x && x == z) triple
  where triple = (,,) <$> wsP (char '<' *> (wsP tagName))
                      <*> wsP (many tagAttr <* char '>')
                      <*> wsP (string "</" *> tagName <* char '>')

-- | Parses all self closing containers, with optional closing tag
selfClosingContainer :: Parser SimpleHTML
selfClosingContainer = semiEmptyContainer <|> emptyContainer


-- | Parses regular containers with open and closing tags, e.g. <div></div>
container :: Parser SimpleHTML -> Parser SimpleHTML
container p = (\(w,x,y,z) -> Element w x y) <$> 
                filter (\(w,x,y,z) -> w == z) quad 
  where quad = (,,,) <$> wsP (char '<' *> (wsP tagName))
                     <*> wsP ((many tagAttr) <* char '>')
                     <*> many p
                     <*> wsP (string "</" *> tagName <* char '>')

-- =====================
-- | HTML element parser
-- =====================

-- | Parses all possiblilities of SimpleHTML
html :: Parser SimpleHTML
html = comment <|> script <|> style <|> selfClosingContainer <|> 
          list <|> container html  <|> pcdata

-- | Parses HTML with possibility of beginning doctype tag and random spaces
htmlPage :: Parser SimpleHTML
htmlPage = wsP (many space *> doc)
  where doc = many (wsP (string "<!doctype html>")) *> html

-- ===================
-- | Utility functions
-- ===================

-- | Grabs the <body> and its nested contents and discards everything else
getBody :: SimpleHTML -> [SimpleHTML]
getBody (PCDATA s) = []
getBody (Comment s) = []
getBody (Element "body" attrs shtmls) = [Element "body" attrs shtmls]
getBody (Element _ _ []) = []
getBody (Element _ _ shtmls) = foldr (\x acc -> getBody x ++ acc) [] shtmls

-- | Check for valid text tag
isTextTag :: String -> Bool
isTextTag s = s `elem` ["p", "h1", "h2", "h3", "h4", "h5", "h6", "li"]


-- | Get text for summarization
getText :: SimpleHTML -> DL.DList Char
getText (PCDATA _) = DL.empty
getText (Comment _) = DL.empty
getText (Element t _ ss) = if isTextTag t
                           then foldr (\x acc -> case (x, acc) of
                             (PCDATA s, DL.Nil) -> DL.fromList s
                             (PCDATA s, acc') -> DL.append (DL.fromList s) acc'
                             (_, acc') -> DL.append (getText x) acc) 
                                DL.empty ss
                           else foldr (\x acc -> case (getText x, acc) of
                             (DL.Nil, DL.Nil) -> acc
                             (DL.Nil, acc')   -> acc'
                             (xs, DL.Nil)     -> xs
                             (xs, acc')       -> DL.append (xs `DL.snoc` ' ') acc')
                                DL.empty ss

-- | Get only absolute links
isValidUrl :: String -> Bool
isValidUrl s = any (\x -> x `isPrefixOf` s) ["http://", "https://", "www"]

-- | Grab all links from the entire HTML page
getLinks :: SimpleHTML -> DL.DList String
getLinks (PCDATA _) = DL.empty
getLinks (Comment _) = DL.empty
getLinks (Element "a" as _) = foldr (\x acc -> case x of
                          (Attribute "href"  v) -> if isValidUrl v
                                                   then v `DL.cons` acc
                                                   else acc
                          _                     -> acc) DL.empty as
getLinks (Element _ _ ss) = foldr (\x acc -> DL.append (getLinks x) acc) 
                              DL.empty ss

-- ============================================================================
-- | PRETTY PRINTNG
-- ============================================================================

-- | Renders input into a string 
render :: PP a => a -> String
render = PP.render . pp

class PP a where
  pp :: a -> Doc

instance PP TagAttribute where
  pp (Attribute n v) = PP.text n PP.<> PP.char '=' PP.<> PP.char '\"' 
                       PP.<> PP.text v PP.<> PP.char '\"'
  
instance PP SimpleHTML where
  pp (PCDATA s) = PP.text s
  pp (Comment s) = PP.text "<!--" PP.<> PP.text s PP.<> PP.text "-->"
  pp (Element t [] []) = open t PP.<> PP.char '>' PP.<> close t
  pp (Element t [] (s:ss)) = open t PP.<> PP.char '>' 
                             PP.<> pp s PP.<> PP.hcat (fmap pp ss) 
                             PP.<> close t
  pp (Element t (a:as) []) = open t PP.<+> pp a PP.<+> PP.hcat (fmap pp as) 
                             PP.<+> PP.char '>' PP.<> close t
  pp (Element t (a:as) (s:ss)) = open t PP.<+> pp a PP.<+> PP.hcat (fmap pp as) 
                                 PP.<+> PP.char '>' PP.<> pp s 
                                 PP.<> PP.hcat (fmap pp ss) PP.<> close t

-- | Function to make unclosed open tags, e.g. <script
open :: String -> Doc
open t = PP.char '<' PP.<> PP.text t

-- | Function to make close tags, e.g. </script>
close :: String -> Doc
close t = PP.text "</" PP.<> PP.text t PP.<> PP.char '>'

-- ============================================================================
-- | TESTS
-- ============================================================================

tHtmlAll :: Test
tHtmlAll = TestList [
    tTagAttr, tPcdata, tComment, tScript, tStyle, tLi, tList, tEmptyContainer
  , tSemiEmptyContainer, tContainer, tHtml, tHtmlPage, tGetText, tGetLinks ]

-- | Separate helper for generatng unquoted tag attributes, because
-- | pretty print only generates quoted attributes
genUnq :: String -> String -> String
genUnq n v = n ++ "=" ++ v

-- | Separate helper for generating empty containers ending in ">", because 
-- | pretty print only generates regular containers
genECon1 :: String -> [TagAttribute] -> [SimpleHTML] -> String
genECon1 t as ss = "<" ++ t ++ " " ++ as' ++ " " ++ ">" ++ ss'
  where as' = if not (null as) then intercalate " " (fmap render as) else []
        ss' = if not (null ss) then intercalate " " (fmap render ss) else []

-- | Separate helper for generating empty containers ending in "/>", because 
-- | pretty print only generates regular containers
genECon2 :: String -> [TagAttribute] -> String
genECon2 t as = "<" ++ t ++ " " ++ as' ++ " />"
  where as' = if not (null as) then intercalate " " (fmap render as) else []
    
tTagAttr :: Test
tTagAttr = TestList [
    doParse tagAttr (render a)   ~?= [(a, "")]
  , doParse tagAttr (genUnq n v) ~?= [(a, "")]
  , doParse tagAttr b            ~?= [(Attribute b "", "")]
  , doParse tagAttr " not valid" ~?= [] ]
  where a = Attribute n v
        n = "src"
        v = "bbc.com/"
        b = "random_boolean"

tPcdata :: Test
tPcdata = TestList [
    doParse pcdata s           ~?= [(PCDATA s, "")]
  , doParse pcdata (s ++ more) ~?= [(PCDATA s, more)]
  , doParse pcdata more        ~?= [] ]
  where s    = "some random text"
        more = "< some momre random text"

tComment :: Test
tComment = TestList [
    doParse comment (render sing)         ~?= [(sing, "")]
  , doParse comment (render mult)         ~?= [(mult, "")]
  , doParse comment (render sing ++ notC) ~?= [(sing, notC)]
  , doParse comment (notC ++ render sing) ~?= [] ]
  where sing = Comment " just a single line comment "
        mult = Comment " a \n\n multi-line\n\n\n comm\n\nent\n\n"
        notC = "not an actual comment"

tScript :: Test
tScript = TestList [
    doParse script (render s) ~?= [(se, "")]
  , doParse script ((render s) ++ notS) ~?= [(se, notS)]
  , doParse script (notS ++ (render s)) ~?= [] ]
  where s    = Element "script" [a] [pc]
        se   = Element "script" [a] []
        a    = Attribute "style" "bombastic"
        pc   = PCDATA "\nrandom gibber1$h :/}<>)12"
        notS = "this is not script stuff"

tStyle :: Test
tStyle = TestList [
    doParse style (render s) ~?= [(se, "")]
  , doParse style ((render s) ++ notS) ~?= [(se, notS)]
  , doParse style (notS ++ (render s)) ~?= [] ]
  where s    = Element "style" [a] [pc]
        se   = Element "style" [a] []
        a    = Attribute "class" "gangnam_style"
        pc   = PCDATA "\n\nsome more r@nd0|\\/| g1bBer%shn"
        notS = "this is not part of style"

tLi :: Test
tLi = TestList [
    doParse li (render l)                          ~?= [(l, "")]
  , doParse li ((render l) ++ notLi)               ~?= [(l, notLi)]
  , doParse li (notLi ++ (render l))               ~?= []
  , doParse li (genECon1 "li" [a] [pc])            ~?= [(l, "")]
  , doParse li ((genECon1 "li" [a] [pc]) ++ notLi) ~?= [(l', "")]
  , doParse li (notLi ++ (genECon1 "li" [a] [pc])) ~?= [] ]
  where l     = Element "li" [a] [pc]
        l'    = Element "li" [a] [PCDATA (isLi  ++ notLi)]
        a     = Attribute "class" "bullet-proof"
        pc    = PCDATA isLi
        isLi  = "When will this testing end"
        notLi = "Not part of this container"


tList :: Test
tList =  TestList [
    doParse list (render ul)           ~?= [(ul, "")]
  , doParse list ((render ul) ++ notL) ~?= [(ul, notL)]
  , doParse list (notL ++ (render ul)) ~?= []
  , doParse list (render ol)           ~?= [(ol, "")]
  , doParse list ((render ol) ++ notL) ~?= [(ol, notL)]
  , doParse list (notL ++ (render ol)) ~?= [] ]
  where ul   = Element "ul" [a] [li1, li2]
        ol   = Element "ol" [a] [li1, li2]
        a    = Attribute "class" "not-another-todo"
        li1  = Element "li" [a] [PCDATA "item1"]
        li2  = Element "li" [a] [PCDATA "item2"]
        notL = "Not part of the list"

tEmptyContainer :: Test
tEmptyContainer = TestList [
    doParse emptyContainer (genECon2 "img" [a1,a2])           ~?= [(e, "")] 
  , doParse emptyContainer ((genECon2 "img" [a1,a2]) ++ notE) ~?= [(e, notE)]
  , doParse emptyContainer (notE ++ (genECon2 "img" [a1,a2])) ~?= [] ]
  where e    = Element "img" [a1,a2] []
        a1   = Attribute "src" "bbc.com/"
        a2   = Attribute "class" "glass_is_half_empty"
        a3   = Attribute "class" "glass is half full"
        notE = "Not part of the container"

tSemiEmptyContainer :: Test
tSemiEmptyContainer = TestList [
    doParse semiEmptyContainer (render p)           ~?= [(p, "")]
  , doParse semiEmptyContainer ((render p) ++ notP) ~?= [(p, notP)]
  , doParse semiEmptyContainer (notP ++ (render p)) ~?= [] ]
  where t    = "path"
        p    = Element "path" [a] []
        a    = Attribute "class" "im_not_a_psychopath"
        notP = "Not part of the container"

tContainer :: Test
tContainer = TestList [
    doParse (container html) (render d)           ~?= [(d, "")]
  , doParse (container html) ((render d) ++ notD) ~?= [(d, notD)]
  , doParse (container html) (notD ++ (render d)) ~?= [] ]
  where d  = Element "div" [a] [h]
        h  = Element "h1" [a] [pc]
        a  = Attribute "class" "cant-box_me"
        pc = PCDATA "hello universe"
        notD = "Not part of the container"

tHtml :: Test
tHtml = TestList [
    doParse html (render d) ~?= [(d, "")]
  , doParse html ((render d) ++ notD) ~?= [(d, notD)]
  , doParse html (notD ++ (render d)) ~?= [(PCDATA notD, (render d))] ]
  where d  = Element "div" [a] [comm, h, e']
        h  = Element "h1" [a] [pc]
        e  = genECon2 "img" [a]
        e' = Element "img" [a] []
        a  = Attribute "class" "cant-box_me"
        pc = PCDATA "hello universe"
        notD = "Not part of the container"
        comm = Comment "Some random comment"

-- | Sample HTML for testing
sHtml :: SimpleHTML
sHtml = Element "div" [Attribute "class" "big"] [
            Element "h1"  [] [PCDATA h1]
          , Element "a"   [Attribute "href" a1] []
          , Element "h2"  [] [PCDATA h2]
          , Element "p"   [] [PCDATA p1]
          , Element "img" [] []
          , Element "a"   [Attribute "href" a2] []
          , Element "p"   [] [PCDATA p2] ]

h1, h2, p1, p2, a1, a2 :: String
h1 = "This is an h1 tag"
h2 = "Hi im h2"
p1 = "Im p1"
p2 = "Im p2"
a1 = "http://cnn.com/"
a2 = "bbc.com/"

tHtmlPage :: Test
tHtmlPage = TestList [
    doParse htmlPage (doc ++ (render sHtml))      ~?= [(sHtml, "")]
  , doParse htmlPage (render sHtml)               ~?= [(sHtml, "")]
  , doParse htmlPage ("\n\n  " ++ (render sHtml)) ~?= [(sHtml, "")] ]
  where doc = "<!DOCTYPE html>"

tGetText :: Test 
tGetText = TestList [
    DL.toList (getText sHtml) ~?= intercalate " " [h1,h2,p1,p2] ]

tGetLinks :: Test
tGetLinks = TestList [
   DL.toList (getLinks sHtml) ~?= [a1] ]

-- ============================================================================
-- | PROPERTY TESTING
-- ============================================================================

arbTagN :: Gen ElementName
arbTagN = listOf1 $ elements (alpha ++ digits)
  where alpha = ['a'..'z']
        digits = ['0'..'9']

arbAttrN :: Gen AttributeName 
arbAttrN = listOf1 $ elements (alpha ++ digits ++ bridges)
  where alpha = ['a'..'z']
        digits = ['0'..'9']
        bridges = ['-', '_', ':']

arbAttrV :: Gen AttributeValue
arbAttrV = arbAttrN

genTagAttr :: Gen TagAttribute
genTagAttr = Attribute <$> arbAttrN <*> arbAttrV

genCom :: Gen SimpleHTML
genCom = Comment <$> arbAttrN

genSHTML :: Int -> Gen SimpleHTML
genSHTML n = frequency [ (1, Comment <$> arbAttrN)
                       , (n, Element <$> arbTagN <*> genTagAttrs n 
                                                 <*> genSHTMLs   n) ] 
  where genTagAttrs 0 = pure []
        genTagAttrs n = (:) <$> genTagAttr <*> genTagAttrs n'
          where n' = n `div` 2
        genSHTMLs 0 = pure []
        genSHTMLs 1 = fmap (\e -> [e]) (PCDATA <$> arbAttrN)
        genSHTMLs n = (:) <$> genSHTML n' <*> genSHTMLs n'
          where n' = n `div` 2
  

instance Arbitrary TagAttribute where
  arbitrary = genTagAttr

instance Arbitrary SimpleHTML where
  arbitrary = sized genSHTML

prop_parseTagAttr :: TagAttribute -> Bool
prop_parseTagAttr t = doParse tagAttr (render t) == [(t, "")]

prop_parseSHTML :: SimpleHTML -> Bool
prop_parseSHTML s = doParse html (render s) == [(s, "")]
