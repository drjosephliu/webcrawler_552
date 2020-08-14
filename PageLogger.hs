module PageLogger where

import Control.Concurrent
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import qualified Data.DList as DL

import Page
import Summarizer

-- ============================================================================
-- | Data types
-- ============================================================================

data PageLogger = PageLogger (MVar PageLogCommand)
data PageLogCommand = LogPage Page
                    | Stop (MVar ())

instance Eq PageLogCommand where
  Stop m1 == Stop m2 = m1 == m2
  LogPage p1 == LogPage p2 = p1 == p2
  _ == _ = False

-- ============================================================================
-- | Methods
-- ============================================================================

-- | Initialises the logger that prints Page contents to file
initLogger :: FilePath -> IO PageLogger 
initLogger fp = do
  m <- newEmptyMVar
  let l = PageLogger m
  forkIO (runLogger l fp)
  return l

-- | Runs the page logger
runLogger :: PageLogger -> FilePath -> IO ()
runLogger (PageLogger m) fp = loop 
  where 
    loop = do
      cmd <- takeMVar m
      case cmd of
        (LogPage pg) -> do
          appendFile fp (show pg)
          loop
        (Stop s) -> do
          putStrLn "STOPPING: File logger."
          putMVar s ()

-- | Logs a page
logPage :: PageLogger -> Page -> IO ()
logPage (PageLogger m) pg = putMVar m (LogPage pg)

-- | Stops the logger
logStop :: PageLogger -> IO ()
logStop (PageLogger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s


-- ============================================================================
-- | Tests
-- ============================================================================


arbStr :: Gen String
arbStr = listOf arbitrary

arbUrl :: Gen Url
arbUrl = do
  s <- arbStr
  return ("http://www." ++ s ++ ".com/")

arbUrlList :: Gen (DL.DList Url)
arbUrlList = DL.fromList <$> (listOf arbUrl)

arbBody :: Gen Body
arbBody = DL.fromList <$> resize 1000 arbStr

arbSummary :: Gen Summary
arbSummary = resize 1000 arbStr

genPage :: Int -> Gen Page
genPage n = frequency [ (1, elements [Empty])
                      , (n, Content <$> arbUrl <*> arbUrlList <*> 
                                       arbBody <*> arbSummary) ]

genStop :: MVar () -> Gen PageLogCommand
genStop stopMVar = oneof [ 
                          pure (Stop stopMVar) ]

instance Arbitrary Page where
  arbitrary = sized genPage

logPageHelper :: Page -> IO PageLogCommand
logPageHelper pg = do
  m <- newEmptyMVar
  let pl = PageLogger m
  logPage pl pg
  cmd <- takeMVar m
  return cmd

prop_logPage :: Page -> Property
prop_logPage pg = monadicIO $ do
  (LogPage pg') <- run $ logPageHelper pg
  assert $ pg' == pg

