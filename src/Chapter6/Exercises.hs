{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter6.Exercises where

import Control.Lens
import Control.Applicative
import Data.Char (isAlpha)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Data.Ord
import Chapter6.Repl

----------------------------------------
-- Simple Folds
----------------------------------------

-- 1

beastSizes :: [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

ex11 = beastSizes ^.. folded
-- [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]
ex12 = beastSizes ^.. folded . folded
-- ["Sirens", "Kraken", "Ogopogo"]
ex13 = beastSizes ^.. folded . folded . folded
-- ["SirensKrakenOgopogo"]
-- "SirensKrakenOgopogo"
ex14 = beastSizes ^.. folded . _2
-- ["Sirens", "Kraken", "Ogopogo"]
ex15 = toListOf (folded . folded) [[1, 2, 3], [4, 5, 6]]
-- [1, 2, 3, 4, 5, 6]
ex16 = toListOf
  (folded . folded)
  (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])
-- "CaptainFist Mate"
ex17 = ("Hello", "It's me") ^.. both . folded
-- "HelloIt's me"
ex18 = ("Why", "So", "Serious?") ^.. each
-- ["Why", "So", "Serious?"]

-- quotes :: [(T.Text, T.Text, T.Text)]
quotes = [("Why", "So", "Serious?"), ("This", "is", "SPARTA")]

ex19 = quotes ^.. each . each . each
-- "WhySoSerious?ThisIsSPARTA"

-- 2
ex21 = toListOf (folded . _1) [(1, 'a'), (2, 'b'), (3, 'c')]
-- [1, 2, 3]
-- folded :: Fold [(Int, Char)] (Int, Char)
-- _1 :: Fold (Int Char) Int

ex22 = toListOf (_2 . folded) (False, S.fromList ["one", "two", "three"])
-- _2 :: Fold (Bool, (S.Set String)) (S.Set String)
-- folded :: Fold (S.Set String) String

ex23 = toListOf
  (folded . folded)
  (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])
-- folded :: Fold (M.Map String String) String
-- folded :: Fold String Char
-- toListOf :: Fold (M.Map String String) Char
--              -> (M.Map String String)
--              -> String

-- 3

-- [1, 2, 3] ^.. _
ex31 = [1, 2, 3] ^.. folded
-- [1, 2, 3]
-- ("Light", "Dark") ^.. _
ex32 = ("Light", "Dark") ^.. _1
-- ["Light"]
-- [("Light", "Dark"), ("Happy", "Sad")] ^.. _
ex33 = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . both
-- ["Light", "Dark", "Happy", "Sad"]
-- [("Light", "Dark"), ("Happy", "Sad")] ^.. _
ex34 = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . _1
-- ["Light, "Happy"]
-- [("Light", "Dark"), ("Happy", "Sad")] ^.. _
ex35 = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . _2 . each
ex35' = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . _2 . folded
-- "DarkSad"
-- ("Bond", "James", "Bond") ^.. _
ex36 = ("Bond", "James", "Bond") ^.. each
-- ["Bond", "James", "Bond"]

----------------------------------------
-- Custom Folds
----------------------------------------

-- 1

-- ["Yer", "a", "wizard", "Harry"] ^.. folded . _
ex6211 = ["Yer", "a", "wizard", "Harry"] ^.. folded . folded
-- ex41 = ["Yer", "a", "wizard", "Harry"] ^. folded
-- YerawizardHarry"

-- [[1, 2, 3], [4, 5, 6]] ^.. folded . _ (take 2)
ex6212 = [[1, 2, 3], [4, 5, 6]] ^.. folded . folding (take 2)
-- [1, 2, 4, 5]

-- [[1, 2, 3], [4, 5, 6]] ^.. folded . _ (take 2)
ex6213 = [[1, 2, 3], [4, 5, 6]] ^.. folded . to (take 2)
-- [[1, 2], [4, 5]]

-- ["bob", "otto", ,"hannah"] ^.. folded . _ reverse
ex6214 = ["bob", "otto", "hannah"] ^.. folded . to reverse
-- ["bob", "otto", "hannah"]

-- ("abc", "def") ^.. _ (\(a, b) -> [a, b]) . _ reverse . _
ex6215 =
  ("abc", "def")
  ^.. folding (\(a, b) -> [a, b])
  . to reverse
  . folded
-- "cbafed"

-- 2

-- [1..5] ^.. _
ex6221 = [1..5] ^.. folded . to (* 100)
-- [100, 200, 300, 400, 500]

-- [(1, "one"), (2, ,"two")] ^.. _
ex6222 = [(1, "one"), (2, "two")] ^.. folded . _2
-- ex6222 = [(1, "one"), (2, "two")] ^.. folded . to snd
-- ["one", "two"]

-- (Just 1, Just 2, Just 3) ^.. _
-- ex6223 :: [Maybe Int]
ex6223 = (Just 1, Just 2, Just 3)
  ^.. folding (\(a, b, c) -> [a, b, c]) . folded
-- ex6223 = (Just 1, Just 2, Just 3)
--   ^.. each
--   . to (\x -> case x of Just n -> n
--                         Nothing -> 0)
-- [1, 2, 3]

-- [Left 1, Right 2, Left 3] ^.. _
ex6224 = [Left 1, Right 2, Left 3]
  ^.. folded
  . folded
-- ex6224 = [Left 1, Right 2, Left 3]
--   ^.. each
--   . _Right
-- [2]

-- [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. _
ex6225 = [([1, 2], [3, 4]), ([5, 6], [7, 8])]
  ^.. folded
  . folding (\(a, b) -> a <> b)
-- ex6225 = [([1, 2], [3, 4]), ([5, 6], [7, 8])]
--   ^.. folded
--   . each
--   . folded
-- [1, 2, 3, 4, 5, 6, 7, 8]

-- [1, 2, 3, 4] ^.. _
ex6226 = [1, 2, 3, 4]
  ^.. folded
  . to (\n -> if odd n then Left n else Right n)
-- [Left 1, Right 2, Left 3, Right 4]

-- [(1, (2, 3)), (4 ,(5, 6))] ^.. _
ex6227 = [(1, (2, 3)), (4 ,(5, 6))]
  ^.. folded
  . folding (\(l, (rl, rr)) -> [l, rl, rr])
-- [1, 2, 3, 4, 5, 6]

-- [(Just 1, Left "one"), (Nothing, Right 2)] ^.. _
ex6228 = [(Just 1, Left "one"), (Nothing, Right 2)]
  ^.. folded
  . folding (\(a, b) -> a ^.. folded <> b ^.. folded)
-- ex6228 = [(Just 1, Left "one"), (Nothing, Right 2)]
--   ^.. folded
--   . to (\(m, e) -> case m of
--                         Just x -> x
--                         Nothing -> case e of
--                                         Right y -> y
--                                         Left _ -> 0)
-- [1, 2]

-- [(1, "one"), (2, "two")] ^.. _
ex6229 = [(1, "one"), (2, "two")]
  ^.. folded
  . folding (\(i, s) -> [Left i, Right s])
-- [Left 1, Right "one", Left 2, Right "two"]

-- S.fromList ["apricots", "apples"] ^.. _
ex62210 = S.fromList ["apricots", "apples"]
  ^.. folded
  . folding reverse
-- "selppastocirpa"

-- 3

-- [(12, 45, 66), (91, 123, 87)] ^.. _
ex6321 = [(12, 45, 66), (91, 123, 87)]
  ^.. folded
  . _2
  . to show
  . folding reverse
-- "54321"

-- [(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^.. _
ex6322 = [(1, "a"), (2, "b"), (3, "c"), (4, "d")]
  ^.. folded
  . folding (\(n, s) -> if even n then [s] else [])
-- ["b", "d"]

----------------------------------------
-- Fold Actions
----------------------------------------

-- _ folded []
ex6411 = has folded []
-- False
-- _ both ("Yo", "Adrian!")
ex6412 = foldOf both ("Yo", "Adrian!")
-- "YoAdrian!"
-- _ each "phones" ("E.T.", "phone", "home")
ex6413 = elemOf each "phone" ("E.T.", "phone", "home")
-- True
-- _ folded [5, 7, 2, 3, 13, 17, 11]
ex6414 = minimumOf folded [5, 7, 2, 3, 13, 17, 11]
-- Just 2
-- _ folded [5, 7, 2, 3, 13, 17, 11]
ex6415 = lastOf folded [5, 7, 2, 3, 13, 17, 11]
-- Just 11
-- _ folded ((> 9) . length) ["Bulbasur", "Charmander", "Squirtle"]
ex6416 = anyOf folded ((> 9) . length) ["Bulbasur", "Charmander", "Squirtle"]
-- True
-- _ folded even [11, 22, 3, 5, 6]
ex6417 = findOf folded even [11, 22, 3, 5, 6]
-- Just 22

ex6421 =
  findOf
    folded
    (\s -> s == reverse s)
    ["umbrella", "olives", "racecar", "hammer"]
-- Just "racecar"
ex6422 =
  allOf
    each
    even
    (2, 4, 6)
-- True
ex6423 =
  maximumByOf
    folded
    (comparing fst)
    [(2, "I'11"), (3, "Be"), (1, "Back")]
-- Just (3, "Be")
ex6424 =
  sumOf
    each
    (1, 2)
-- 3
ex6431 =
  maximumByOf
    folded
    (comparing length)
    $ words "Do or do not, there is no try"
ex6431' =
  maximumByOf
    (folding words)
    (comparing (length . filter (`elem` "aeiouy")))
    "Do or do not, there is no try"
ex6432 =
  foldrOf
    folded
    (flip (<>))
    mempty
    ["a", "b", "c"]
-- "cba"
ex6433 =
  [(12, 45, 66), (91, 123, 87)]
    ^.. folded
      . _2
      . to (reverse . show)
      . folded
-- "54321"
ex6434 =
  [(1, "a"), (2, "b"), (3, "c"), (4, "d")]
    ^.. folded
      . folding (\(a, b) -> if even a then pure b
                                      else [])
-- ["b", "d"]

----------------------------------------
-- Higher Order Folds
----------------------------------------

-- "Here's looking at you, kid" ^.. _ 7 folded
ex6511 = "Here's looking at you, kid" ^.. dropping 7 folded
-- "looking at you, kid"
-- ["My Precious", "Hakuna Matata", "No problemo"]
--  ^.. folded . taking 1 _
ex6512 = ["My Precious", "Hakuna Matata", "No problemo"]
--           ^.. folded . taking 1 (folding words)
           ^.. folded . taking 1 worded
-- ["My", "Hakuna", "No"]
-- ["My Precious", "Hakuna Matata", "No problemo"]
--  ^.. _
ex6513 = ["My Precious", "Hakuna Matata", "No problemo"]
--           ^.. taking 1 (folded . folding words)
           ^.. taking 1 (folded . worded)
-- ["My"]
-- ["My Precious", "Hakuna Matata", "No problemo"]
--  ^.. folded . _
ex6514 = ["My Precious", "Hakuna Matata", "No problemo"]
--           ^.. folded . taking 1 (folding words) . folded
           ^.. folded . taking 1 worded . folded
-- "MyHakunaNo"
-- _ (10, 50, 100)
ex6515 = sumOf (taking 2 each) (10, 50, 100)
-- 60
-- ("stressed", "guns", "evil") ^.. _ each
ex6516 = ("stressed", "guns", "evil") ^.. backwards each
-- ["evil", "guns", "stressed"]
-- ("stressed", "guns", "evil") ^.. backwards each . to _
ex6517 = ("stressed", "guns", "evil") ^.. backwards each . to reverse
-- ["live", "snug", "desserts"]
-- "blink182 k9 blazeit420" ^.. _
ex6518 = "blink182 k9 blazeit420"
--            ^.. (folding words . droppingWhile isAlpha each)
            ^.. worded . droppingWhile isAlpha folded
-- 1829420

sample :: [Int]
sample = [ -10, -5, 4, 3, 8, 6, -2, 3, -5, -7]

ex6521 = lengthOf (takingWhile (<=0) folded) sample
-- 2
ex6522 = maximumOf (taking 4 folded) sample
-- Just 4
ex6523 = sample ^? dropping 1 (droppingWhile (/=4) folded)
-- Just 3
ex6524 = lengthOf (takingWhile (<0) (backwards folded)) sample
-- 2
ex6525 = sample ^.. (takingWhile  (>0) (droppingWhile (<0) folded))
-- [4, 3, 8, 6]
ex6526 = sample
  ^.. backwards (droppingWhile (<0) (backwards (droppingWhile (<0) folded)))
-- [4, 3, 8, 6, -2, 3]

trimmingWhile :: (a -> Bool) -> Fold s a -> Fold s a
trimmingWhile p f =
--  backwards (droppingWhile p (backwards (droppingWhile p f)))
  backwards
  . droppingWhile p
  . backwards
  . droppingWhile p
  $ f

ex6526' = sample ^.. trimmingWhile (<0) folded

----------------------------------------
-- Filtering
----------------------------------------

ex6611 = deck
           ^.. folded
             . filtered ((== 'S') . head . _name'')
ex6611' = deck
            ^.. folded
              . filteredBy (name'' . (taking 1 folded) . (only 'S'))
ex6611'' = deck
             ^.. folded
               .name''
               . filtered ((== 'S') . head)
ex6612 = minimumOf
           (folded . moves . folded . movePower)
           deck
ex6612' = minimumByOf
            (folded . moves . folded)
            (comparing _movePower)
            deck
ex6613 = deck
           ^.. taking 1 ( folded
                        . (filtered ((>1) . length . _moves))
                        . name'')
ex6613' = deck
            ^? folded
             -- . (filtered ((>1) . length . _moves))
             . (filteredBy (moves . filtered ((>1) . length)))
             . name''
ex6614 = deck
           ^.. folded
             . filteredBy (aura . (only Hot))
             . filteredBy (moves . folded . movePower . (filtered (>30)))
ex6614' = anyOf
            ( folded
            . filteredBy (aura . only Hot)
            . moves
            . folded
            . movePower)
            (>30)
            deck
ex6615 = deck
           ^.. folded
             . filtered _holo
             . filteredBy (aura . only Wet)
             . name''
ex6616 = sumOf
           ( folded
           . filtered ((/= Leafy) . _aura)
           . moves
           . folded
           . movePower)
           deck
