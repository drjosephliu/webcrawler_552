module Corpus where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Control.Monad as Monad
import System.Directory
import System.IO 
import System.FilePath ((</>))

import Test.HUnit
import Test.QuickCheck

import Summarizer

-- | Corpus Reader Methods
-- ============================================================================
getFiles :: Monad m => (a -> m [a]) -> a -> m [a]
getFiles children root = do
                          xs <- children root
                          sub <- mapM (getFiles children) xs
                          return $ root : concat sub

-- | Get a list of files in directory
topFileList :: FilePath -> IO [FilePath]
topFileList path = fmap (map (path </>)) $ listDirectory path

-- | Method to get all paths of txt files nested in a directory
getAllFiles :: FilePath -> IO [FilePath]
getAllFiles = getFiles children
  where children path = do
                          directory <- doesDirectoryExist path
                          if directory
                            then topFileList path
                            else return []

getDocumentFromFile :: FilePath -> IO Document
getDocumentFromFile file = do 
                            x <- readFile file
                            return $ splitDocument x

isTxtFile :: FilePath -> IO Bool
isTxtFile f = do
                  x <- doesFileExist f
                  let y = ".txt" `List.isSuffixOf` f
                  return $ x && y

-- | Read all text files from a directory and store in corpus
readCorpus :: String -> IO Corpus
readCorpus dir = do 
                  x <- getAllFiles dir
                  y <- Monad.filterM isTxtFile x
                  fmap Corp (mapM getDocumentFromFile y)

testCorpus :: IO Corpus
testCorpus = readCorpus "test"

-- | Corpus Reader Tests
-- ============================================================================

