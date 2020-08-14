module Summarizer where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Control.Monad as Monad
import Data.List.Split

import Test.HUnit
import Test.QuickCheck

 -- | Data types
-- ============================================================================

type Summary = String

type Text = String

data Sentence = Sentence [Text] Text
  deriving (Eq, Show)
newtype Document = Doc [Sentence]
  deriving (Eq, Show)
newtype Corpus = Corp [Document]
  deriving (Eq, Show)

-- | Calcluation Helper Methods
-- ============================================================================

-- | Calculate a term frequency score
tf :: (Floating a) => a -> a -> a
tf term all = term / all

-- | Calculate an inverse document frequency score
idf :: (Eq a, Floating a) => a -> a -> a
idf term doc = case (term, doc) of 
                    (_, 0) -> 0
                    (0, _) -> 0
                    (t, d) -> log(d / t)

-- | Text Normalization Helper Methods
-- ============================================================================

-- | Takes a string of chars to filter from string
clean :: String -> String -> String
clean str xs = [ x | x <- xs, x `notElem` str ]

-- | Trim white space from string
trim :: String -> String
trim = List.dropWhileEnd Char.isSpace . List.dropWhile Char.isSpace

-- | Summarize Methods
-- ============================================================================

-- | Takes document string and splits into a Document
splitDocument :: String -> Document
splitDocument str = 
  Doc $ map (\ x -> Sentence (splitWords x) x) getSentences
  where getSentences = map cleanSentence sentences
        sentences = split (dropBlanks $ keepDelimsR $ oneOf ".?!") str
        splitWords x = map cleanWord (words x)
        cleanWord word = map Char.toLower (clean ",.?!-:;\"\'" word)
        cleanSentence = trim . clean "\n\t"

-- | Goes through a sentence counting the frequency of all the words
analyzeSentence :: Sentence -> Map.Map Text Double
analyzeSentence (Sentence s _) = 
  foldr (\ x xs -> Map.insertWith (+) x 1 xs) Map.empty s

-- | Goes through a document counting the frequency of all the words
analyzeDocument :: Document -> Map.Map Text Double
analyzeDocument (Doc d) = map
  where map = Map.unionsWith (+) (foldr (\x xs -> analyzeSentence x : xs) [] d)

-- | Goes through a document counting the frequency of all the words
-- | and the number of documents the words appear in stored in a tuple
analyzeCorpus :: Corpus -> Map.Map Text Double
analyzeCorpus (Corp c) = corp
  where corp = Map.unionsWith (+)(map doc c)
        doc x = Map.foldrWithKey 
                  (\k x xs -> Map.insert k 1 xs) 
                  Map.empty (analyzeDocument x)

-- | Analyze all Documents and output list of tuples (sentence text,scores) based on
-- | tf-idf score of sentence
analyze :: Corpus -> Document -> [(Text, Double)]
analyze (Corp c) (Doc d) = map (\ x -> (getSentence x, sentenceScore x)) d
  where getSentence (Sentence _ s) = s
        corpMap = analyzeCorpus (Corp c)
        docMap = analyzeDocument (Doc d)
        sentenceScore (Sentence s str) = foldr tfidf 0 s / fromIntegral (length s)
          where tfidf term xs = tf (Map.findWithDefault 0 term docMap) 
                                    (fromIntegral $ Map.size docMap) * 
                                idf (Map.findWithDefault 0 term corpMap) 
                                    (fromIntegral $ length c) + xs

-- | Extract first i sentences of Summary and convert back into a string
extractSummary :: Int -> [(Text, Double)]-> Summary
extractSummary i sentences = getSentences i (List.sortBy sort sentences)
  where sort (a1,b1) (a2,b2) = case compare b1 b2 of
                                  EQ -> compare a1 a2
                                  LT -> GT
                                  GT -> LT
        getSentences 0 _          = ""
        getSentences i []         = ""
        getSentences i ((x,_):xs) = x ++ " " ++ getSentences (i-1) xs

-- | Extract first i sentences of Summary and convert back into a string
summarize' :: Int -> String -> Corpus -> Summary
summarize' i str corpus = extractSummary i results
  where results = analyze corpus (splitDocument str)

summarize :: Int -> String -> IO Corpus -> IO Summary
summarize i str corpus = do 
                            c <- corpus
                            let results = analyze c (splitDocument str)
                            return $ extractSummary i results

 -- | Tests
-- ============================================================================

testTf :: Test
testTf = TestList [
    "tf1 " ~: tf 1 2 ~?=  0.5,
    "tf2 " ~: tf 4 1 ~?=  4,
    "tf3 " ~: tf 0 1 ~?=  0,
    "tf4 " ~: tf 1 0 ~?=  0 ]

testIdf :: Test
testIdf = TestList [
    "idf1 " ~: idf 2 2 ~?= 0, 
    "idf2 " ~: idf 0 1 ~?= 0,
    "idf3 " ~: idf 1 0 ~?= 0,
    "idf4 " ~: idf 0 0 ~?= 0,
    "idf5 " ~: idf 1 2 ~?= 0.6931471805599453,
    "idf6 " ~: idf 2 1 ~?= -0.6931471805599453]

testSplit :: Test
testSplit = TestList [
    "split1 " ~: splitDocument "Hello. My name is Michelle." ~?= 
        Doc [Sentence ["Hello"] "hello", 
             Sentence ["my", "name", "is", "michelle"] "My name is Michelle"],
    "split2 " ~: splitDocument "Hello." ~?= Doc [Sentence ["hello"] "Hello"],
    "split3 " ~: splitDocument "" ~?= Doc []]

testAnalyzeDocument :: Test
testAnalyzeDocument = TestList [
    "analyzeDoc1 " ~: analyzeDocument doc1 ~?= 
      Map.fromList [("Hello",1), ("My",1), ("name",1), ("is",1), ("Michelle",1)],
    "analyzeDoc2 " ~: analyzeDocument doc2 ~?= 
      Map.fromList [("the",1), ("dog",2), ("is",1), ("a",1)],
    "analyzeDoc3 " ~: analyzeDocument (Doc []) ~?= Map.fromList []]
      where doc1 = Doc [Sentence ["Hello"] "Hello", 
                        Sentence ["My", "name", "is", "Michelle"] "My name is Michelle"]
            doc2 = Doc [Sentence ["the", "dog", "is", "a", "dog"] "the dog is a dog"]

testAnalyze :: Test 
testAnalyze = TestList [
    "analyze1 " ~: analyze c1 d1 ~?= 
      [("sales boost the euro",2.4141923738573524e-2),
       (" The dollar is set",3.418357719946078e-2),
       (" profits are hit",9.987384442437362e-2)],
    "analyze2 " ~: analyze c1 (Doc []) ~?= []]


article1 = "Dollar gains on Greenspan speech. The dollar has hit its highest level against the euro in almost three months after the Federal Reserve head said the US trade deficit is set to stabilise. And Alan Greenspan highlighted the US government's willingness to curb spending and rising household savings as factors which may help to reduce it. In late trading in New York, the dollar reached $1.2871 against the euro, from $1.2974 on Thursday. Market concerns about the deficit has hit the greenback in recent months. On Friday, Federal Reserve chairman Mr Greenspan's speech in London ahead of the meeting of G7 finance ministers sent the dollar higher after it had earlier tumbled on the back of worse-than-expected US jobs data. \"I think the chairman's taking a much more sanguine view on the current account deficit than he's taken for some time,\" said Robert Sinche, head of currency strategy at Bank of America in New York. \"He's taking a longer-term view, laying out a set of conditions under which the current account deficit can improve this year and next.\" Worries about the deficit concerns about China do, however, remain. China's currency remains pegged to the dollar and the US currency's sharp falls in recent months have therefore made Chinese export prices highly competitive. But calls for a shift in Beijing's policy have fallen on deaf ears, despite recent comments in a major Chinese newspaper that the \"time is ripe\" for a loosening of the peg. The G7 meeting is thought unlikely to produce any meaningful movement in Chinese policy. In the meantime, the US Federal Reserve's decision on 2 February to boost interest rates by a quarter of a point - the sixth such move in as many months - has opened up a differential with European rates. The half-point window, some believe, could be enough to keep US assets looking more attractive, and could help prop up the dollar. The recent falls have partly been the result of big budget deficits, as well as the US's yawning current account gap, both of which need to be funded by the buying of US bonds and assets by foreign firms and governments. The White House will announce its budget on Monday, and many commentators believe the deficit will remain at close to half a trillion dollars."
article2 = "Ad sales boost Time Warner profit. Quarterly profits at US media giant TimeWarner jumped 76% to $1.13bn (£600m) for the three months to December, from $639m year-earlier. The firm, which is now one of the biggest investors in Google, benefited from sales of high-speed internet connections and higher advert sales. TimeWarner said fourth quarter sales rose 2% to $11.1bn from $10.9bn. Its profits were buoyed by one-off gains which offset a profit dip at Warner Bros, and less users for AOL. Time Warner said on Friday that it now owns 8% of search-engine Google. But its own internet business, AOL, had has mixed fortunes. It lost 464,000 subscribers in the fourth quarter profits were lower than in the preceding three quarters. However, the company said AOL's underlying profit before exceptional items rose 8% on the back of stronger internet advertising revenues. It hopes to increase subscribers by offering the online service free to TimeWarner internet customers and will try to sign up AOL's existing customers for high-speed broadband. TimeWarner also has to restate 2000 and 2003 results following a probe by the US Securities Exchange Commission (SEC), which is close to concluding. Time Warner's fourth quarter profits were slightly better than analysts' expectations. But its film division saw profits slump 27% to $284m, helped by box-office flops Alexander and Catwoman, a sharp contrast to year-earlier, when the third and final film in the Lord of the Rings trilogy boosted results. For the full-year, TimeWarner posted a profit of $3.36bn, up 27% from its 2003 performance, while revenues grew 6.4% to $42.09bn. \"Our financial performance was strong, meeting or exceeding all of our full-year objectives and greatly enhancing our flexibility,\" chairman and chief executive Richard Parsons said. For 2005, TimeWarner is projecting operating earnings growth of around 5%, and also expects higher revenue and wider profit margins. TimeWarner is to restate its accounts as part of efforts to resolve an inquiry into AOL by US market regulators. It has already offered to pay $300m to settle charges, in a deal that is under review by the SEC. The company said it was unable to estimate the amount it needed to set aside for legal reserves, which it previously set at $500m. It intends to adjust the way it accounts for a deal with German music publisher Bertelsmann's purchase of a stake in AOL Europe, which it had reported as advertising revenue. It will now book the sale of its stake in AOL Europe as a loss on the value of that stake."
article3 = "Yukos unit buyer faces loan claim. The owners of embattled Russian oil giant Yukos are to ask the buyer of its former production unit to pay back a $900m (£479m) loan. State-owned Rosneft bought the Yugansk unit for $9.3bn in a sale forced by Russia to part settle a $27.5bn tax claim against Yukos. Yukos' owner Menatep Group says it will ask Rosneft to repay a loan that Yugansk had secured on its assets. Rosneft already faces a similar $540m repayment demand from foreign banks. Legal experts said Rosneft's purchase of Yugansk would include such obligations. \"The pledged assets are with Rosneft, so it will have to pay real money to the creditors to avoid seizure of Yugansk assets,\" said Moscow-based US lawyer Jamie Firestone, who is not connected to the case. Menatep Group's managing director Tim Osborne told the Reuters news agency: \"If they default, we will fight them where the rule of law exists under the international arbitration clauses of the credit.\" Rosneft officials were unavailable for comment. But the company has said it intends to take action against Menatep to recover some of the tax claims and debts owed by Yugansk. Yukos had filed for bankruptcy protection in a US court in an attempt to prevent the forced sale of its main production arm. The sale went ahead in December and Yugansk was sold to a little-known shell company which in turn was bought by Rosneft. Yukos claims its downfall was punishment for the political ambitions of its founder Mikhail Khodorkovsky and has vowed to sue any participant in the sale."
c1 = Corp [splitDocument article1, splitDocument article2, splitDocument article3]
d1 = splitDocument "sales boost the euro. The dollar is set. profits are hit."



