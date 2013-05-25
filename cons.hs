----------------------------------------------------------------
-- cons - QuickCheck Example
-- Generate a random sentence. Use Lisp List Version.
-- Copyright (C) 2013 by @quasicrane (Twitter)
----------------------------------------------------------------
module EnglishRule1 where
import Test.QuickCheck
import Control.Monad
import Data.List

data Object = Nil | S String | (:.) {car, cdr :: Object} deriving (Eq)

instance Show Object where
    show Nil = "()"
    show (S x) = x
    show (x :. y) = showCons (x :. y)

-- see HyperSpec "22.1.3.5 Printing Lists And Conses"
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

-- list operator
cons = (:.)
list1 x = x :. Nil
list2 x y = x :. (y :. Nil)
list3 x y z = x :. (y :. (z :. Nil))
list4 w x y z = w :. (x :. (y :. (z :. Nil)))
makeList = foldr cons Nil

consp Nil = False
consp (S _) = False
consp (_ :. _) = True

atom Nil = True
atom (S _) = True
atom (_ :. _) = False

listp x = consp x || x == Nil

-- car (x :. _) = x
-- car x | not (consp x) = error ("car expect cons but got: " ++ show x)

-- cdr (_ :. y) = y
-- cdr Nil = Nil
-- cdr x | not (consp x) = error ("cdr expect cons but got: " ++ show x)

caar x = car (car x)
cadr x = car (cdr x)
cddr x = cdr (cdr x)

assoc :: Object -> Object -> Object
assoc item alist
    | alist == Nil = Nil
    | not (consp alist) = error ("assoc expect list but got: " ++ show alist)
    | not (consp (car alist)) = error ("assoc expect cons but got: " ++ show (car alist))
    | caar alist == item = car alist
    | caar alist /= item = assoc item (cdr alist)

-- | Utility. Generate Lisp List from Haskell Array
array2list = foldr cons Nil
-- | Utility. Generate Haskell List from Lisp Array
list2array Nil = []
list2array (x :. y) 
    | listp y = x : list2array y
    | otherwise = error ("list2array expect list, but got: " ++ show y)

-- FIXME use apply
append x y
    | x == Nil = y
    | otherwise = cons (car x) (append (cdr x) y)

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- map :: (a -> b) -> [a] -> [b]
-- mapcar :: (Object -> Object) -> Object -> Object
-- mapcar' :: Monad m => (Object -> m Object) -> Object -> m Object
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

mapcar f list
    | list == Nil = Nil
    | otherwise = cons (f (car list)) (mapcar f (cdr list))

-- mapcar' :: Monad m => (Object -> m Object) -> Object -> m Object
mapcar' f list
    | list == Nil = return Nil
    | otherwise = liftM2 cons (f (car list)) (mapcar' f (cdr list))

mappend :: (Object -> Object) -> Object -> Object
mappend f list
    | list == Nil = Nil
    | otherwise = append (f (car list)) (mappend f (cdr list))

-- | Apply fn to each element of list and append the results.
mappend' f list
    | list == Nil = return Nil
    | otherwise = liftM2 append (f (car list)) (mappend' f (cdr list)) 

-- | Generate a random sentence or phrase.
generate :: Object -> Object -> Gen Object
generate grm phrase
    | listp phrase = mappend' g phrase
    | rewrites grm phrase /= Nil =
        do
          y <- randomElt (rewrites grm phrase)
          g y
    | otherwise = return $ list1 phrase
    where
      g = generate grm

-- | Generate a random sentence or phrase wiht a complete parse tree.
generateTree :: Object -> Object -> Gen Object
generateTree grm phrase
    | listp phrase = mapcar' g phrase
    | rewrites grm phrase /= Nil =
        do
          t <- liftM g (randomElt (rewrites grm phrase))
          liftM (cons phrase) t
    | otherwise = return $ list1 phrase
    where
      g = generateTree grm

-- | sample data
ex grm phrase = do
  xs <- sample' $ generate grm phrase
  return $ head xs

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


