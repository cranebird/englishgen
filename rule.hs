----------------------------------------------------------------
-- simple.hs - QuickCheck Example
-- Generate a random sentence.
-- Copyright (C) 2013 by @quasicrane
----------------------------------------------------------------
-- | Generate a random sentence.
-- See 2.3 A Rule-Based Solution in PAIP (Paradigms of Artificial Intelligence Programming).
module EnglishGen where
import Test.QuickCheck
import Control.Monad
import Data.Tree

data Rule = Empty | Rule String [Rule] | Word String [String] | Rule :+ Rule
            deriving (Eq, Show)

type Grammar = [Rule]

-- Utility
-- | Concatenate strings.
cat "" "" = ""
cat x "" = x
cat "" y = y
cat x y = x ++ " " ++ y

simpleGrammar = [ noun, verb, article, nounPhrase, verbPhrase, sentence ] :: Grammar
    where
      sentence = Rule "sentence" [nounPhrase :+ verbPhrase]
      noun = Word "noun" ["man", "ball", "woman", "table"]
      verb = Word "verb" ["hit", "took", "saw", "liked"]
      article = Word "article" ["the", "a"]
      nounPhrase = Rule "noun-phrase" [article :+ noun]
      verbPhrase = Rule "verb-phrase" [verb :+ nounPhrase]

bigGrammar = [ sentence, nounPhrase, verbPhrase, ppStar, adjStar,
               pp, prep, adj, article, name, noun, verb, pronoun ] :: Grammar
    where
      sentence = Rule "sentence" [nounPhrase :+ verbPhrase]
      nounPhrase = Rule "noun-phrase" [article :+ adjStar :+ noun :+ ppStar, name, pronoun]
      verbPhrase = Rule "verb-phrase" [verb :+ nounPhrase :+ ppStar]
      ppStar = Rule "pp*" [Empty, pp :+ ppStar]
      adjStar = Rule "adj*" [Empty, adj :+ adjStar]
      pp = Rule "pp" [prep :+ nounPhrase]
      prep = Word "prep" ["to", "in", "by", "with", "on"]
      adj = Word "adj" ["big", "little", "blue", "green", "adiabatic"]
      article = Word "article" ["the", "a"]
      name = Word "name" ["Pat", "Kim", "Lee", "Terry", "Robin"]
      noun = Word "noun" ["man", "ball", "woman", "table"]
      verb = Word "verb" ["hit", "took", "saw", "liked"]
      pronoun = Word "pronoun" ["he", "she", "it", "these", "those", "that"]

lambdaGrammar = [ exp ] :: Grammar
    where
      var = Word "var" ["x", "y", "z"]
      hat = Word "lambda" ["^"]
      dot = Word "lambda" ["."]
      l = Word "lparen" ["("]
      r = Word "rparen" [")"]
      abs = Rule "abs" [hat :+ var :+ dot :+ exp]
      app = Rule "app" [l :+ exp :+ exp :+ r]
      exp = Rule "exp" [var, abs, app]

lispGrammar = [ exp ] :: Grammar
    where
      var = Word "var" [ "x", "y", "z" ]
      sym = Word "sym" [ "+", "-", "*", "/" ]
      letsym = Word "letsym" ["let", "let*", "letrec"]
      l = Word "lparen" [ "(" ]
      r = Word "rparen" [ ")" ]
      fn = Rule "fn" [l :+ sym :+ var :+ exp :+ r]
      bind = Rule "bind" [l :+ var :+ fn :+ r]
      bindStar= Rule "bind*" [Empty, bind :+ bindStar]
      letexp = Rule "letexp" [l :+ letsym :+ l :+ bindStar :+ r :+ var :+ r]
      exp = Rule "exp" [var, letexp, fn]

-- | Lookup Rule in Grammar.
lookupRule :: String -> Grammar -> Rule
lookupRule x [] = error ("fail to lookupRule: " ++ x)
lookupRule x (r:rs) =
    case r of
      Rule y _ -> if x == y then
                      r
                  else
                      lookupRule x rs
      Word y _ -> if x == y then
                      r
                  else
                      lookupRule x rs
      _ -> lookupRule x rs

-- | Generate a random sentence.
--
-- >>> sample' $ generate simpleGrammar "sentence"
-- ["a table took the table","the ball saw a table","a woman took the woman","the man saw a ball","a ball liked a woman","a woman hit the ball","a ball liked the ball","the man hit a man","the woman saw the woman","the ball saw a ball","a woman liked the man"]
generate :: Grammar -> String -> Gen String
generate rs phrase = mkGen $ lookupRule phrase rs

mkGen :: Rule -> Gen String
mkGen (Rule x rs) = oneof (map mkGen rs)
mkGen (Word x ws) = elements ws
mkGen Empty = return ""
mkGen (r1 :+ r2) =
    do
      s1 <- mkGen r1
      s2 <- mkGen r2
      return $ cat s1 s2

-- | Generate a list of all possible expansions of this phrase.
--
-- >>> generateAll simpleGrammar "noun"
-- ["man","ball","woman","table"]
-- >>> generateAll simpleGrammar "noun-phrase"
-- ["the man","the ball","the woman","the table","a man","a ball","a woman","a table"]
-- >>> length $ generateAll simpleGrammar "sentence"
-- 256
generateAll :: Grammar -> String -> [String]
generateAll rs phrase = mkAll $ lookupRule phrase rs

mkAll :: Rule -> [String]
mkAll (Rule x rs) = concatMap mkAll rs  
mkAll (Word x ws) = ws
mkAll Empty = []
mkAll (r1 :+ r2) = [ cat s1 s2 | s1 <- mkAll r1, s2 <- mkAll r2]

-- Data.Tree Version
-- | Generate a random sentence with a complete parse tree.
generateTree :: Grammar -> String -> Gen (Forest String)
generateTree rs phrase = mkGenTree $ lookupRule phrase rs

mkGenTree :: Rule -> Gen (Forest String)
mkGenTree (Rule x rs) = 
    do
      t <- oneof (map mkGenTree rs)
      return [Node x t]

mkGenTree (Word x ws) =
    do
      a <- elements ws
      return [ Node x [Node a []]]

mkGenTree Empty = return []
mkGenTree (r1 :+ r2) =
    do
      t1 <- mkGenTree r1
      t2 <- mkGenTree r2
      return $ t1 ++ t2

-- | Draw a Sentence Parse Tree
--
-- >>> draw simpleGrammar "noun-phrase"
-- noun-phrase
-- |
-- +- article
-- |  |
-- |  `- the
-- |
-- `- noun
--    |
--    `- table
-- >>> draw bigGrammar "sentence"
-- sentence
-- |
-- +- noun-phrase
-- |  |
-- |  `- name
-- |     |
-- |     `- Kim
-- |
-- `- verb-phrase
--    |
--    +- verb
--    |  |
--    |  `- liked
--    |
--    +- noun-phrase
--    |  |
--    |  `- pronoun
--    |     |
--    |     `- that
--    |
--    `- pp*
--       |
--       `- 
draw rs phrase = do
  ts <- sample' $ generateTree rs phrase
  putStrLn (drawForest (head ts))

