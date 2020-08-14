module WebCrawler where 

import Test.HUnit
import Test.QuickCheck

import Control.Concurrent
import qualified Data.DList as DL
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import qualified Data.DateTime as DateTime
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8

import Summarizer
import Corpus
import Html
import Parser
import Page
import qualified Logger as L
import qualified PageLogger as PL

-- ============================================================================
-- | Data types
-- ============================================================================

type UrlQueue = Chan Url

-- ============================================================================
-- | Crawl Methods
-- ============================================================================

-- | Start URL
startUrl :: Url
startUrl = "https://www.cnn.com/2019/12/05/politics/donald-trump-impeachment-strategy/index.html"

-- | Retrieves links, content body and summary of the visited URL
getContent :: Url -> IO Page
getContent url = do
  request <- parseRequest url
  response <- httpLBS request
  let htmlStr = (L8.unpack . getResponseBody) response
  let obj = doParse htmlPage htmlStr
    in case obj of
      [] -> return Empty
      _  -> do
        corpus <- testCorpus
        let body = getBody ((fst . head) obj)
            text  = getText (head body)
            links = getLinks (head body)
            summary = summarize' 5 (DL.toList text) corpus
         in return (Content url links text summary)

-- | Adds URLs to queue
addUrls :: DL.DList Url -> UrlQueue -> L.Logger -> IO ()
addUrls DL.Nil q l = return ()
addUrls (DL.Cons u us) q l = do
  writeChan q u
  L.logMessage l ("Adding URL: " ++ u)
  addUrls (DL.fromList us) q l

-- | Fetches contents from URL and adds new URLs to the queue
fetch :: Url -> UrlQueue -> PL.PageLogger -> L.Logger -> IO ()
fetch url q pl l = do
  pg <- getContent url
  case pg of
    Empty -> return ()
    (Content u us b s) -> do
      PL.logPage pl pg
      addUrls us q l

-- | Grabs URL from head of queue and forks new thread to fetch its contents
urlGrabber :: UrlQueue -> PL.PageLogger -> L.Logger -> IO ()
urlGrabber q pl l = do
  url <- readChan q
  L.logMessage l ("Next URL is: " ++ url)
  forkIO $ fetch url q pl l
  urlGrabber q pl l

-- | Initialises crawler in a new thread
initCrawler :: Url -> PL.PageLogger -> L.Logger -> IO UrlQueue
initCrawler url pl l = do
  q <- newChan
  writeChan q url
  forkIO $ urlGrabber q pl l
  return q

-- | Initialises loggers and starts the crawler
startCrawler :: IO ()
startCrawler = do
  putStrLn $ "Type a file name to write to: "
  outFileName <- getLine
  putStrLn $ "Type in an initial URL to begin crawling: "
  startUrl <- getLine
  l <- L.initLogger
  pl <- PL.initLogger outFileName
  initCrawler startUrl pl l
  loop l pl
  where 
    loop l' pl' = do
        cmd <- getLine
        if cmd == "stop" || cmd == "exit"
        then do
          L.logStop l'
          PL.logStop pl'
          putStrLn "Shutting down crawler. 'Till next time."
          return ()
        else loop l' pl'

