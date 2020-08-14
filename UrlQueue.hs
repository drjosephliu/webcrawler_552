module UrlQueue where

import Control.Monad
import Control.Concurrent
import qualified Data.DList as DL

import WebCrawler
import Logger as L
import PageLogger as PL

type Url2 = String
type UrlQueue2 = Chan Url
type OutFile = MVar FilePath

addLinks :: [Url2] -> UrlQueue2 -> Logger -> IO ()
addLinks [] q l = return ()
addLinks (u:us) q l = do
    writeChan q u
    L.logMessage l ("Adding URL: " ++ u)
    addLinks us q l

fetch :: Url2 -> UrlQueue2 -> PL.PageLogger -> L.Logger -> IO ()
fetch url q pl l = do
    pg <- getContent url
    case pg of
        Empty -> return ()
        (Content u us b s) -> do
            PL.logPage pl pg
            addLinks (DL.toList us) q l

reader :: UrlQueue2 -> PL.PageLogger -> L.Logger -> IO ()
reader q pl l = do
    url <- readChan q
    L.logMessage l ("Next URL is: " ++ url)
    forkIO $ fetch url q pl l
    reader q pl l

initCrawler :: Url -> PL.PageLogger -> L.Logger -> IO UrlQueue2
initCrawler url l pl = do
  q <- newChan
  writeChan q url
  forkIO $ reader q pl l
  return q

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (runLogger l)
  return l

start :: IO ()
start = do
    putStrLn $ "Type in a file to write to: "
    outFileName <- getLine
    putStrLn $ "Type in an initial URL to begin crawling: "
    startUrl <- getLine
    l <- L.initLogger
    pl <- PL.initLogger outFileName
    initCrawler startUrl pl l
    loop l pl
    where loop :: Logger -> PageLogger -> IO ()
          loop l' pl' = do
          cmd <- getLine
          if cmd == "stop" || cmd == "exit"
          then do
            L.logStop l'
            PL.logStop pl'
            putrStrLn "Shutting down crawler. `Till next time."
            return ()
          else loop l' pl'

main :: IO ()
main = do
  forkIO start
  loop
  where loop = do
        cmd <- getLine
        if cmd == "stop" || cmd == "exit"
        then return ()
        else loop
    
