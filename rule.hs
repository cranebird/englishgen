----------------------------------------------------------------
-- simple.hs - QuickCheck Example
-- Generate a random sentence.
-- Copyright (C) 2013 by @quasicrane (Twitter)
----------------------------------------------------------------
-- | Generate a random sentence.
-- See 2.3 A Rule-Based Solution in PAIP (Paradigms of Artificial Intelligence Programming).

module EnglishGen where
import Test.QuickCheck
import Control.Monad

type RuleName = String
data Rule = Rule [[RuleName]] | Word [String]
type Grammar = [(RuleName, Rule)]
type Grammar' = [(RuleName, Gen String)]

-- | Concatenate strings.
cat "" "" = ""
cat x "" = x
cat "" y = y
cat x y = x ++ " " ++ y

-- | Combine two Gen String.
(.+) :: Gen String -> Gen String -> Gen String
a .+ b = liftM2 cat a b

rewrites :: Grammar -> String -> Rule
rewrites grm phrase = 
    case lookup phrase grm of
      Just r -> r
      Nothing -> error $ "phrase not found: " ++ phrase

-- FIXME listp は使えない。型が合わない。
-- generate :: Grammar -> [String] -> Gen String
-- generate grm xs = foldr (.+) (return "") (map (generate grm) xs)

mkgen :: Grammar -> String -> Gen String
mkgen grm phrase = mkgen' grm phrase (rewrites grm phrase)

mkgen' grm _ (Word ws) = elements ws
mkgen' grm x (Rule rs) = oneof $ map (rule2gen grm x) rs

rule2gen :: Grammar -> String -> [String] -> Gen String
rule2gen grm x [] = return ""
rule2gen grm x (r:rs) = mkgen grm r .+ rule2gen grm x rs

simpleGrammar = [ ("sentence", Rule [["noun-phrase", "verb-phrase"]]),
                  ("article", Word ["the", "a"]),
                  ("noun", Word ["man", "ball", "woman", "table"]),
                  ("verb", Word ["hit", "took", "saw", "liked"] ),
                  ("noun-phrase", Rule [["article", "noun"]]),
                  ("verb-phrase", Rule [["verb", "noun-phrase"]])
                ]  :: Grammar

bigGrammar = [ ("sentence", Rule [["noun-phrase", "verb-phrase"]]),
               ("noun-phrase", Rule [["article", "adj*", "noun", "pp*"],
                                     ["name"],
                                     ["pronoun"]]),
               ("verb-phrase", Rule [["verb", "noun-phrase", "pp*"]]),
               ("pp*", Rule [[], ["pp", "pp*"]]),
               ("adj*", Rule [[], ["adj", "adj*"]]),
               ("pp", Rule [["prep", "noun-phrase"]]),
               ("prep", Word ["to", "in", "by", "with", "on"]),
               ("adj", Word ["big", "little", "blue", "green", "adiabatic"]),
               ("article", Word ["the", "a"]),
               ("name", Word ["Pat", "Kim", "Lee", "Terry", "Robin"]),
               ("noun", Word ["man", "ball", "woman", "table"]),
               ("verb", Word ["hit", "took", "saw", "liked"]),
               ("pronoun", Word ["he", "she", "it", "these", "those", "that"]) ] :: Grammar


