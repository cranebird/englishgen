----------------------------------------------------------------
-- cons.hs - QuickCheck Example
-- Use Lisp List as Tree ver.
-- Copyright (C) 2013 by @quasicrane (Twitter)
----------------------------------------------------------------
-- | "Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp"
-- Random English phrase generator in "2.3 A Rule-Based Solution" in Haskell.
module EnglishGen where
import Test.QuickCheck
import Control.Monad
import Data.List

data Object
    = Nil -- ^ nil Object
    | S String -- ^ Symbol
    | Object :. Object -- ^ Cons
    deriving (Eq)

instance Show Object where
    show Nil = "()"
    show (S x) = x
    show (x :. y) = showCons (x :. y)

-- | Show Cons. See HyperSpec "22.1.3.5 Printing Lists And Conses"
--
-- >>> cons (S "x") Nil
-- (x)
-- >>> cons (S "x") (S "y")
-- (x . y)
-- >>> cons (S "a") (cons (S "x") (S "y"))
-- (a x . y)
-- >>> cons (S "x") (cons (S "y") Nil)
-- (x y)
showCons x = step1 x ""
    where
      step1 x res = step2 x (res ++ "(")
      step2 x res = step3 x (res ++ show (car x))
      step3 x res = if consp (cdr x)
                    then
                        step2 (cdr x) (res ++ " ")
                    else
                        step4 x res
      step4 x res = if cdr x /= Nil
                    then
                        step5 x (res ++ " . " ++ show (cdr x))
                    else
                        step5 x res
      step5 x res = res ++ ")"

-- | Create a cons.
--
-- >>> cons (S "a") (S "b")
-- (a . b)
cons = (:.)
-- | Create a list.
--
-- >>> list1 (S "x")
-- (x)
list1 x = x :. Nil
list2 x y = x :. (y :. Nil)
list3 x y z = x :. (y :. (z :. Nil))
list4 w x y z = w :. (x :. (y :. (z :. Nil)))
-- | Return True if object is a cons. Otherwise, return False.
consp Nil = False
consp (S _) = False
consp (_ :. _) = True
-- | Return True if object is Nil or symbol. Otherwise, return False.
atom Nil = True
atom (S _) = True
atom (_ :. _) = False
-- | Return True if object is consp or Nil. Otherwise, return False.
listp x = consp x || x == Nil

-- | Return car part of cons.
car (x :. _) = x
car x | not (consp x) = error ("car expect cons but got: " ++ show x)

-- | Return cdr part of cons.
cdr (_ :. y) = y
cdr Nil = Nil
cdr x | not (consp x) = error ("cdr expect cons but got: " ++ show x)
-- | Return car of car of an object.
caar x = car (car x)
-- | Return the first cons in alist whose car equal item, or nil if no such cons is found.
assoc :: Object -> Object -> Object
assoc item alist
    | alist == Nil = Nil
    | not (consp alist) = error ("assoc expect list but got: " ++ show alist)
    | not (consp (car alist)) =
        error ("assoc expect cons but got: " ++ show (car alist))
    | caar alist == item = car alist
    | caar alist /= item = assoc item (cdr alist)

-- | Generate Lisp List from Haskell Array.
array2list = foldr cons Nil
-- | Generate Haskell List from Lisp Array.
list2array Nil = []
list2array (x :. y) 
    | listp y = x : list2array y
    | otherwise = error ("list2array expect list, but got: " ++ show y)

-- | Return list concatenation of lists.
--
-- >>> append (list3 (S "a") (S "b") (S "c")) (list2 (S "x") (S "y"))
-- (a b c x y)
append x y
    | x == Nil = y
    | otherwise = cons (car x) (append (cdr x) y)
-- | Mapping operation. apply function f to elements of list.
mapcar f list
    | list == Nil = Nil
    | otherwise = cons (f (car list)) (mapcar f (cdr list))

-- | Mapping operation. apply function f to elements of list.
-- like mapcar, but function f is monadic.
mapcar' f list
    | list == Nil = return Nil
    | otherwise = liftM2 cons (f (car list)) (mapcar' f (cdr list))

-- | Apply fn to each element of list and append the results.
mappend :: (Object -> Object) -> Object -> Object
mappend f list
    | list == Nil = Nil
    | otherwise = append (f (car list)) (mappend f (cdr list))

-- | Apply fn to each element of list and append the results.
-- like mappend, but function f is monadic.
mappend' f list
    | list == Nil = return Nil
    | otherwise = liftM2 append (f (car list)) (mappend' f (cdr list)) 

-- | Grammar as alist.
type Grammar = Object
-- | Generate a random sentence or phrase.
--
-- >>> sample' $ generate simpleGrammar (S "noun-phrase")
-- [(a woman),(a woman),(the ball),(the ball),(the man),(the table),(a ball),(a ball),(a table),(a man),(the man)]
generate :: Grammar
         -> Object
         -> Gen Object
generate grm phrase
    | listp phrase = mappend' g phrase
    | rewrites grm phrase /= Nil =
        do
          y <- randomElt (rewrites grm phrase)
          g y
    | otherwise = return $ list1 phrase
    where
      g = generate grm

-- | Generate a random sentence or phrase with a complete parse tree.
--
-- >>> sample' $ generateTree simpleGrammar (S "noun-phrase")
-- [(noun-phrase (article the) (noun ball)),(noun-phrase (article a) (noun table)),(noun-phrase (article a) (noun man)),(noun-phrase (article the) (noun ball)),(noun-phrase (article the) (noun woman)),(noun-phrase (article a) (noun woman)),(noun-phrase (article a) (noun ball)),(noun-phrase (article a) (noun table)),(noun-phrase (article a) (noun woman)),(noun-phrase (article a) (noun man)),(noun-phrase (article a) (noun ball))]
generateTree :: Grammar -> Object -> Gen Object
generateTree grm phrase
    | listp phrase = mapcar' g phrase
    | rewrites grm phrase /= Nil =
        do
          t <- liftM g (randomElt (rewrites grm phrase))
          liftM (cons phrase) t
    | otherwise = return $ list1 phrase
    where
      g = generateTree grm

-- | Generate a sample data.
ex grm phrase = do
  xs <- sample' $ generate grm phrase
  return $ head xs
-- | Generate a sample tree data.
ex' grm phrase = do
  xs <- sample' $ generateTree grm phrase
  return $ head xs

-- | The left-hand side of a rule.
ruleLhs = car
-- | The right-hand side of a rule.
ruleRhs rule = cdr (cdr rule)
-- | Return a list of the possible rewrites for this category.
rewrites grm category
    | atom category = ruleRhs (assoc category grm)
    | otherwise = error ("rewrite got: " ++ show category)

-- | Pick an element from a list at random.
oneOf set = do
  x <- randomElt set
  return $ list1 x

-- | Choose an element from a list at random.
randomElt :: Object -> Gen Object
randomElt choices
    | listp choices = elements (list2array choices)
    | otherwise = error ("randomElt expect choices but got: "  ++ show choices)

-- | Big Grammar.
bigGrammar = array2list [
              d "sentence" [["noun-phrase", "verb-phrase"]],
              d "noun-phrase" [["article", "adj*", "noun", "pp*"],
                               ["name"],
                               ["pronoun"]],
              d "verb-phrase" [["verb", "noun-phrase", "pp*"]],
              d "pp*" [[], ["pp", "pp*"]],
              d "adj*" [[], ["adj", "adj*"]],
              d "pp" [["prep", "noun-phrase"]],
              w "prep" ["to", "in", "by", "with", "on"],
              w "adj" ["big", "little", "blue", "green", "adiabatic"],
              w "article" ["the", "a"],
              w "name" ["Pat", "Kim", "Lee", "Terry", "Robin"],
              w "noun" ["man", "ball", "woman", "table"],
              w "verb" ["hit", "took", "saw", "liked"],
              w "pronoun" ["he", "she", "it", "these", "those", "that"]
             ]
    where
      d x xs = S x :. (S "->" :. array2list (map (array2list . map S) xs))
      w x xs = S x :. (S "->" :. array2list (map S xs))

-- | Simple Grammar.
simpleGrammar = array2list [
                 d "sentence" [["noun-phrase", "verb-phrase"]],
                 d "noun-phrase" [["article", "noun"]],
                 d "verb-phrase" [["verb", "noun-phrase"]],
                 w "article" ["the", "a"],
                 w "noun" ["man", "ball", "woman", "table"],
                 w "verb" ["hit", "took", "saw", "linked"]
                ]
    where
      d x xs = S x :. (S "->" :. array2list (map (array2list . map S) xs))
      w x xs = S x :. (S "->" :. array2list (map S xs))
