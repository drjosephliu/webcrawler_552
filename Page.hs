module Page where

import qualified Data.DList as DL

import Summarizer

-- ============================================================================
-- | Data types
-- ============================================================================

data Page = Content Url (DL.DList Url) Body Summary
          | Empty

type Url = String
type Body = DL.DList Char

-- ============================================================================
-- | Instances
-- ============================================================================

bar = 
 "\n\t========================================================================"
  ++ "========================================================================"

sbar = 
 "\n\t------------------------------------------------------------------------"
  ++ "------------------------------------------------------------------------"

-- | Allows for prettier formatting
instance Show Page where
  show Empty = ""
  show (Content u us b s) = bar ++ "\n\tCONTENT: " ++ u ++ ",\n" ++ "\t[\n" ++ 
                            (pprint (DL.toList us)) ++ "\t],\n" ++ 
                            "\tBODY: " ++ (DL.toList b) ++ ",\n" ++ sbar ++ 
                            "\n\tSUMMARY: " ++ s ++ "\n"
    where pprint l = (unlines . map (\x -> '\t':'\t':x)) l

instance Eq Page where
  Empty == Empty = True
  Content u1 us1 b1 s1 == Content u2 us2 b2 s2 = u1  == u2 &&
                                                 us1 == us2 &&
                                                 b1  == b2 &&
                                                 s1  == s2
  _ == _ = False
