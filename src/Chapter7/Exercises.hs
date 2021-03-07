{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter7.Exercises where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Data.Char (toUpper, toLower)
import Control.Monad.State
import Chapter7.Repl

----------------------------------------
-- Simple Traversals
----------------------------------------

-- ex7111
-- Fold
-- ex7112
-- Either
-- ex7113
-- Lens, traversal or fold

-- ("Jurassic", "Park") & _ .~ "N/A"
ex7121 = ("Jurassic", "Park") & both .~ "N/A"
-- ("N/A", "N/A")
-- ("Jurassic", "Park") & both . _ .~ 'x'
ex7122 = ("Jurassic", "Park") & both . traversed .~ 'x'
ex7122' = ("Jurassic", "Park") & both . each .~ 'x'
-- ("xxxxxxxx", "xxxx")
-- ("Malcolm", ["Kaylee", "Inara", "Jayne"])
--   & _ id traversed %~ take 3
ex7123 = ("Malcolm", ["Kaylee", "Inara", "Jayne"])
           & beside id traversed %~ take 3
-- ("Mal", ["Kay", "Ina", "Jay"])
-- ("Malcolm", ["Kaylee", "Inara", "Jayne"])
--   & _2 . _1 .~ "River"
ex7124 = ("Malcolm", ["Kaylee", "Inara", "Jayne"])
           & _2 . element 1 .~ "River"
-- ("Malcom", ["Kaylee", "River", "Jayne"])
-- ["Die Another Day", "Live and Let Die", "You Only Live Twice"]
--   & traversed . _ _ 1 . traversed .~ 'x'
ex7125 =
  ["Die Another Day", "Live and Let Die", "You Only Live Twice"]
    & traversed . elementOf worded 1 . traversed .~ 'x'
-- [ "Die xxxxxxx Day"
-- , "Live xxx Let Die"
-- , "You xxxx Live Twice"
-- ]
-- ((1, 2), (3, 4)) & _ +~ 1
ex7126 = ((1, 2), (3, 4)) & each . each +~ 1
ex7126' = ((1, 2), (3, 4)) & both . both +~ 1
-- ((2, 3), (4, 5))
-- (1, (2, [3, 4])) & _ +~ 1
ex7127 = (1, (2, [3, 4])) & beside id (beside id traversed) +~ 1
-- (2, (3, [4, 5]))
-- ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries"))
--   & _
--   %~ toUpper
ex7128 =
  ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries"))
    & each
    . filteredBy _1
    -- . filtered fst
    . _2
    . taking 5 traversed
    %~ toUpper
-- ((True, "STRAWberries"), (False, "Blueberries"), (True, "BLACKberries"))
-- ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries"))
--   & _
ex7129 = 
  ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries"))
    & each
    %~ snd
-- ("Strawberries", "Blueberries", "Blackberries")

----------------------------------------
-- Traversal Actions
----------------------------------------

-- _ _1 (Nothing, "Rosebud")
ex7211 = sequenceAOf _1 (Nothing, "Rosebud")
-- Nothing
-- sequenceAOf (traversed . _1) _
ex7212 = sequenceAOf (traversed . _1) [ ("ab", 1), ("cd", 2) ]
-- [ [('a', 1) ,('c', 2)]
-- , [('a', 1) ,('d', 2)]
-- , [('b', 1) ,('c', 2)]
-- , [('b', 1) ,('d', 2)]]
-- sequenceAOf _ [ZipList [1, 2], ZipList [3, 4]]
ex7213 = sequenceAOf traversed [ZipList [1, 2], ZipList [3, 4]]
-- ZipList { getZipList = [[1, 3], [2, 4]] }
-- sequenceAOf (traversed . _2) [('a', ZipList _), ('b', ZipList _)]
ex7214 = sequenceAOf (traversed . _2)
                     [ ('a', ZipList [1, 2])
                     , ('b', ZipList [3, 4])]
-- ZipList { getZipList = [[('a', 1), ('b', 3)], [('a', 2), ('b', 4]] }
-- result = traverseOf _
--            (\n -> modify (+n) >> get)
--            ([1, 1, 1], (1, 1))
result = traverseOf (beside traversed both)
           (\n -> modify (+n) >> get)
           ([1, 1, 1], (1, 1))

ex7215 = runState result 0
-- (([1, 2, 3], (4, 5)), 5)

ex7221 = traverseOf
           (_1 . traversed)
           (\c -> [toLower c, toUpper c])
           ("ab", True)
-- [ ("ab", True)
-- , ("aB", True)
-- , ("Ab", True)
-- , ("AB", True)
-- ]
ex7221' = ("ab", True)
            & _1 . traversed
            %%~ (\c -> [toLower c, toUpper c])
ex7222 = traverseOf
           (traversed . _1)
           (\c -> [toLower c, toUpper c])
           [('a', True), ('b', False)]
-- [ [('a',True), ('b',False)]
-- , [('a',True), ('B',False)]
-- , [('A',True), ('b',False)]
-- , [('A',True), ('B',False)]
-- ]
ex7222' = [('a', True), ('b', False)]
            & traversed . _1
           %%~ (\c -> [toLower c, toUpper c])

data User =
  User { _name :: String
       , _age :: Int
       } deriving Show

makeLenses ''User


data Account =
  Account { _id :: String
          , _user :: User
          } deriving Show

makeLenses ''Account

validateAge :: Account -> Either String Account
validateAge = traverseOf (user . age) check
  where check n | n < 0 = Left "A bit young to have an account here aren't ya?"
                | n > 150 = Left "A bit old to have an account here aren't ya?"
                | otherwise = Right n

validateAge' :: Account -> Either String Account
validateAge' account = account
                        & user . age
                        %%~ (\a -> if a > 0 && a < 150
                                   then Right a
                                   else Left "Invalid age")

----------------------------------------
-- Custom Traversals
----------------------------------------

amountT :: Traversal' Transaction Int
amountT handler (Withdrawal n) = Withdrawal <$> handler n
-- amountT fn (Withdrawal amt) = liftA Withdrawal (fn amt)
amountT fn (Deposit amt) = liftA Deposit (fn amt)

both' :: Traversal (a, a) (b, b) a b
both' fn (x, y) = liftA2 (,) (fn x) (fn y)

transactionDelta :: Traversal' Transaction Int
transactionDelta fn (Deposit amt) = liftA Deposit (fn amt)
-- transactionDelta fn (Withdrawal amt) =
--   liftA Withdrawal (fn . negate $ amt)
transactionDelta handler (Withdrawal n) =
  Withdrawal . negate <$> handler (negate n)

ex7331 = Deposit 10 ^? transactionDelta
-- Just 10
ex7332 = Withdrawal 10 ^? transactionDelta
-- Just (-10)
ex7333 = Deposit 10 & transactionDelta .~ 15
-- Deposit { _amount = 15 }
ex7334 = Withdrawal 10 & transactionDelta .~ 15
-- Withdrawal { _amount = 15 }
ex7335 = Deposit 10 & transactionDelta +~ 5
-- Deposit { _amount = 15 }
ex7336 = Withdrawal 10 & transactionDelta +~ 5
-- Withdrawal { _amount = 5 }

left :: Traversal (Either a b) (Either a' b) a a'
left fn (Left a) = liftA Left (fn a)
left _ (Right b) = pure $ Right b

beside' :: Traversal s t a b
       -> Traversal s' t' a b
       -> Traversal (s, s') (t, t') a b
beside' left' right' handler (s, s') =
  liftA2 (,) (s & left' %%~ handler) (s' & right' %%~ handler)

----------------------------------------
-- Traversal Laws
----------------------------------------

-- Breaks second law
ex7411 = "w1 w2" & worded %~ (++ "   a")
                 & worded %~ (++ "b")
ex7411' = "w1 w2" & worded %~ (++ "b") . (++ "   a")

ex7421 = error "DO"

ex7431 = error "DO"

-- taking: lawful
-- ex7441
-- beside: Lawful
-- ex7442
-- each: lawful
-- ex7443
-- lined: unlawful, similar to worded
-- ex7444
-- traversed: lawful
-- ex7445

----------------------------------------
-- partsOf
----------------------------------------

-- [1, 2, 3, 4] ^. partsOf (traversed . filtered even)
ex7511 = [1, 2, 3, 4] ^. partsOf (traversed . filtered even)
--[2, 4]
-- ["Aardvark", "Bandicoot", "Capybara"]
--   ^. traversed . partsOf (taking 3 traversed)
ex7512 = ["Aardvark", "Bandicoot", "Capybara"]
           ^. traversed . partsOf (taking 3 traversed)
-- "AarBanCap"
-- ([1, 2], M.fromList [('a', 3), ('b', 4)])
--   ^. partsOf _
ex7513 = ([1, 2], M.fromList [('a', 3), ('b', 4)])
           ^. partsOf (beside traversed traversed)
-- [1, 2, 3, 4]
-- [1, 2, 3, 4]
--   & partsOf (traversed . _)
--   .~ [20, 40]
ex7514 = [1, 2, 3, 4]
           & partsOf (traversed . filtered even)
           .~ [20, 40]
-- [1, 20, 3, 40]
-- ["Aardvark", "Bandicoot", "Capybara"]
--   & partsOf _
--   .~ "Kangaroo"
ex7515 = ["Aardvark", "Bandicoot", "Capybara"]
           & partsOf (traversed . traversed)
           .~ "Kangaroo"
-- ["Kangaroo", "Bandicoot", "Capybara"]
-- ["Aardvark", "Bandicoot", "Capybara"]
--   & partsOf (traversed . traversed)
--   .~ "Ant"
ex7516 = ["Aardvark", "Bandicoot", "Capybara"]
           & partsOf (traversed . traversed)
           .~ "Ant"
-- ["Antdvark", "Bandicoot", "Capybara"]
-- M.fromList [('a', 'a'), ('b', 'b'), ('c', 'c')]
--   & partsOf traversed
--   %~ _
ex7517 = M.fromList [('a', 'a'), ('b', 'b'), ('c', 'c')]
           & partsOf traversed
           %~ \(x:xs) -> xs ++ [x]
-- M.fromList [('a', 'b'), ('b', 'c'), ('c', 'a')]
-- ('a', 'b', 'c')
--   & _
--   %~ reverse
ex7518 = ('a', 'b', 'c')
           & partsOf each
           %~ reverse
-- ('c', 'b', 'a')
-- [1, 2, 3, 4, 5, 6]
--   & partsOf _
--   %~ reverse
ex7519 = [1, 2, 3, 4, 5, 6]
           & partsOf (taking 3 traversed)
           %~ reverse
-- [3, 2, 1, 4, 5, 6]
-- ('a', 'b', 'c')
--   & _ each
--   %~ \xs -> fmap ((,) xs) xs
ex7520 = ('a', 'b', 'c')
           & unsafePartsOf each
           %~ \xs -> fmap ((,) xs) xs
-- (("abc", 'a'), ("abc", 'b'), ("abc", 'c'))

