module Crawl where

{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import Html
import Parser
import Summarizer
import Corpus

type Url = String
type Body = String
data Page = Content Url [Url] Body
  deriving (Eq, Show)

pageLinks (Content _ l _) = l
pageText (Content _ _ t) = t

-- | Function to retrieve link and content from a Url
getContent :: Url -> IO Page
getContent url = do
    request <- parseRequest url
    response <- httpLBS request
    let htmlStr = (L8.unpack . getResponseBody) response
    let body = getBody ((fst . head) $ doParse htmlPage htmlStr)
    let text = getText (head body)
    let links = getLinks (head body)
    return $ Content url links text

-- | Function to start and complete the crawl after i pages are crawled
crawl :: [Url] -> Int -> IO [Page]
crawl [] _ = return $ []
crawl _ 0 = return $ []
crawl (x:xs) i = do 
            page <- getContent x
            --xss <- xs ++ (getLinks page)
            pages <- crawl xs (i-1)
            return $ page:pages

execute :: Url -> Int -> Int -> Corpus -> IO [Summary]
execute seed limit size corpus =
    do
    pages <- crawl [seed] limit
    text <- map pageText pages
    summaries <- map (\x -> summarize' size x corpus) text
    return $ ["Hello"]



