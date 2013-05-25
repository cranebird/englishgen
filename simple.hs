----------------------------------------------------------------
-- simple.hs - QuickCheck Example
-- Generate a random sentence.
-- Copyright (C) 2013 by @quasicrane (Twitter)
----------------------------------------------------------------
-- | Generate a radom sentence.
-- See 2.2 A Straightforward solution in PAIP (Paradigms of Artificial Intelligence Programming).

module EnglishGen where
import Test.QuickCheck

-- | Generate a random sentence.
--
-- >>> sample' sentence
-- ["the ball saw the ball","a man saw a table","the woman saw a table","a woman hit the woman","a table took the ball","the man saw the man","a table liked a table","a woman saw a man","the ball saw the table","the woman saw a table","a man saw the man"]
sentence =
    do
      np <- nounPhrase
      vp <- verbPhrase
      return $ unwords [np, vp]

-- | Generate a random noun-phrase.
--
-- >>> sample' nounPhrase
-- ["the ball","the man","a ball","the table","a man","a woman","a woman","a woman","a man","a table","the table"]
nounPhrase =
    do
      a <- article
      n <- noun
      return $ unwords [a, n]
-- | Generate a random verb-phrase.
--
-- >>> sample' verbPhrase
-- ["hit the woman","saw the ball","saw a woman","took the woman","hit a woman","liked the ball","saw the man","liked the table","hit a woman","took the table","liked the woman"]
verbPhrase =
    do
      v <- verb
      np <- nounPhrase
      return $ unwords [v, np]

-- | Generate a random article.
-- 
-- >>> sample' article
-- ["the","the","the","a","a","a","a","the","a","the","a"]
article = elements ["the", "a"]

-- | Generate a random noun.
--
-- >>> sample' noun
-- ["woman","woman","table","ball","ball","woman","table","table","table","woman","woman"]
noun = elements ["man", "ball", "woman", "table"]

-- | Generate a random verb.
--
-- >>> sample' verb
-- ["took","hit","took","liked","took","saw","saw","liked","took","hit","saw"]
verb = elements ["hit", "took", "saw", "liked"]


