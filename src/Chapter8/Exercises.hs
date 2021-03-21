{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter8.Exercises where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

----------------------------------------
-- Indexable Structures
----------------------------------------

-- ["Larry", "Curly", "Moe"]
--   & _ 1
--   .~ "Wiggly"
ex8111 = ["Larry", "Curly", "Moe"]
           & ix 1
           .~ "Wiggly"
-- ["Larry", "Wiggly", "Moe"]
heroesAndVilains =
  M.fromList [("Superman", "Lex"), ("Batman", "Joker")]
-- heroesAndVilains & _ "SpiderMan" .~ Just "Goblin"
ex8112 = heroesAndVilains
          & at "SpiderMan"
          .~ Just "Goblin"
-- M.fromList [("Superman", "Lex"), ("SpiderMan", "Goblin"), ("Batman", "Joker")]
-- _ "Superman" heroesAndVilains
ex8113 = sans "Superman" heroesAndVilains
-- M.fromList [("Batman", "Joker")]
-- S.fromList ['a', 'e', 'i', 'o', 'u']
--   & at 'y' _ ()
--   & at 'i' .~ _
ex8114 = S.fromList ['a', 'e', 'i', 'o', 'u']
           & at 'y' ?~ ()
           & at 'i' .~ Nothing
-- S.fromList "aeouy"

input = M.fromList [("candy bars", 13), ("soda", 34), ("gum", 7)]
output = M.fromList [("candy bars", 13), ("ice cream", 5), ("soda", 37)]
ex8121 = input
           & ix "soda" +~ 3
           & at "ice cream" ?~ 5
           -- & at "gum" .~ Nothing
           & sans "gum"

----------------------------------------
-- Custom Indexed Structures
----------------------------------------

newtype CaseInsensitive a = CaseInsensitive {_unCaseSensitive :: M.Map String a}
  deriving Show

makeLenses ''CaseInsensitive

type instance Index (CaseInsensitive a) = String
type instance IxValue (CaseInsensitive a) = a

instance Ixed (CaseInsensitive a) where
  ix :: String -> Traversal' (CaseInsensitive v) v
  ix k = unCaseSensitive . ix (map toLower k)

instance At (CaseInsensitive a) where
  at :: String -> Lens' (CaseInsensitive a) (Maybe a)
  at k = unCaseSensitive . at (map toLower k)

----------------------------------------
-- Missing Values
----------------------------------------

optic = ix "first" `failing` ix "second"
ex8311 = M.fromList [("first", False), ("second", False)]
           & optic .~ True
-- M.fromList [("first", True), ("second", False)]
ex8312 = M.fromList [("second", False)]
           & optic .~ True
-- M.fromList [("second", True)]

optic2 = _1 . filtered even `failing` _2
ex8321 = (1, 1) & optic2 *~ 10
-- (1, 10)
ex8322 = (2, 2) & optic2 *~ 10
-- (20, 2)

optic3 = traversed . filtered even `failing` traversed
ex8331 = [1, 2, 3, 4] ^.. optic3
-- [2 ,4]
ex8332 = [1, 3, 5] ^.. optic3
-- [1, 3, 5]

ex8341 = Nothing ^. non "default"
-- "default"
-- ex8342 = Nothing & _ +~ 7
ex8342 = Nothing & non 100 +~ 7
-- Just 107
-- ex8343 = M.fromList [("Perogies", True), ("Pizza", True), ("Pilsners", True)]
--            ^. at "Broccoli" . _
ex8343 = M.fromList [("Perogies", True), ("Pizza", True), ("Pilsners", True)]
           ^. at "Broccoli" . non False
-- False
-- ex8344 = M.fromList [("Breath of the wild", 22000000), ("Odyssey", 9070000)]
--            & _ +~ 999
ex8344 = M.fromList [("Breath of the wild", 22000000), ("Odyssey", 9070000)]
           & at "Wario's Woods" . non 0 +~ 999
-- M.fromList
--   [ ("Breath of the wild", 22000000)
--   , ("Odyssey", 9070000)
--   , ("Wario's Woods", 999)
--   ]
-- ex8345 = ["Math", "Science", "Geography"]
--            ^. _ . non "Unscheduled"
ex8345 = [("Math" :: String), "Science", "Geography"]
           ^. pre (ix 4) . non "Unscheduled"
-- "Unscheduled"

-- ex8351 = [1, 2, 3, 4] ^.. _
ex8351 = [1, 2, 3, 4] ^.. traversed . pre (filtered even) . non (-1)
-- [-1, 2, -1, 4]
