module Logger where

import Control.Concurrent
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)

-- ============================================================================
-- | Data types
-- ============================================================================

data Logger = Logger (MVar LogCommand)
data LogCommand = Message String
                | Stop (MVar ())

-- ============================================================================
-- | Methods
-- ============================================================================

-- | Initialise logger that prints to stdout
initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (runLogger l)
  return l

-- | Runs the logger
runLogger :: Logger -> IO ()
runLogger (Logger m) = loop 
  where 
    loop = do
      cmd <- takeMVar m
      case cmd of
        (Message msg) -> do
          putStrLn msg
          loop
        (Stop s) -> do
          putStrLn "STOPPING: Stdout logger."
          putMVar s ()

-- | Logs a message
logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

-- | Stops the logger
logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

-- ============================================================================
-- | Tests
-- ============================================================================

arbStr :: Gen String
arbStr = listOf arbitrary

logMessageHelper :: String -> IO LogCommand
logMessageHelper msg = do
  m <- newEmptyMVar
  let l = Logger m
  logMessage l msg
  cmd <- takeMVar m
  return cmd

prop_logMessage :: String -> Property
prop_logMessage msg = monadicIO $ do
  (Message msg') <- run $ logMessageHelper msg
  assert $ msg == msg'
