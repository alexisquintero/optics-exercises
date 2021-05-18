{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter10.Exercises where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Numeric.Lens
import Data.List (transpose, sortOn)
import Data.List.NonEmpty hiding (filter, tail, transpose, zip)
import Data.List.NonEmpty (nonEmpty)-- hiding (filter, tail, transpose, zip)
import Data.Char (isUpper, toUpper, toLower)

----------------------------------------
-- Intro to Isos
----------------------------------------

ex10111 = "Iso"
ex10112 = "Traversal"
ex10113 = "Prism"
ex10114 = "Iso"
ex10115 = "Traversal"
ex10116 = "Iso"

-- ex10121 = ("Beauty", "Age") ^. _
ex10121 = ("Beauty", "Age") ^. swapped
-- ("Age", ,"Beauty")
-- ex10122 = 50 ^. _ (adding 10)
ex10122 = 50 ^. from (adding 10)
-- 40
-- ex10123 = 0 & multiplying _ +~ 12
ex10123 = 0 & multiplying 4 +~ 12
-- 3.0
-- ex10124 = 0 & adding 10 . multiplying 2 .~ _
ex10124 = 0 & adding 10 . multiplying 2 .~ 24
-- 2
-- ex10125 = [1, 2, 3] & reversed %~ _
-- ex10125 = [1, 2, 3] & reversed %~ dropWhile odd
ex10125 = [1, 2, 3] & reversed %~ tail
-- [1, 2]
-- ex10126 = (view _ (++)) [1, 2] [3, 4]
ex10126 = (view flipped (++)) [1, 2] [3, 4]
-- [3, 4, 1, 2]
-- ex10127 = [1, 2, 3] _ reversed
ex10127 = [1, 2, 3] ^. reversed
-- [3, 2, 1]
-- ex10128 = [[1, 2, 3], [10, 20, 30]] & involuted transpose %~ _
ex10128 = [[1, 2, 3], [10, 20, 30]] & involuted transpose %~ id
-- [[1, 10], [2, 20], [3, 30]]
switchCase c = if isUpper c then toLower c else toUpper c
-- ex10129 = (32, "Hi") & _2 . _ .~ ("hELLO" :: String)
ex10129 = (32, "Hi") & _2 . involuted (fmap switchCase) .~ ("hELLO" :: String)
-- (32, "Hello")

celsiusToF :: Double -> Double
celsiusToF c = (c * (9/5)) + 32

fahrenheit :: Iso' Double Double
fahrenheit = iso celsiusToF fahrenheitToC
  where
    fahrenheitToC f = (f - 32) * (5/9)

----------------------------------------
-- Projected Isos
----------------------------------------

-- ex10211 = ("Beauty", "Age") ^. mapping reversed . _
ex10211 = (("Beauty" :: String), ("Age" :: String)) ^. mapping reversed . swapped
-- ("egA", "Beauty")
-- ex10212 = [True, False, True] ^. mapping (_ not)
ex10212 = [True, False, True] ^. mapping (involuted not)
-- [False, True, False]
-- ex10213 = [True, False, True] & _ %~ filter id
ex10213 = [True, False, True] & mapping (involuted not) %~ filter id
-- [False]
ex10214 = (show ^. mapping reversed) 1234
-- ex10214 = (show ^. _) 1234
-- "4321"

intNot :: Int -> Int
intNot = not ^. dimapping enum (from enum)

ex10221 = intNot 0
-- 1
ex10222 = intNot 1
-- 0
ex10223 = intNot 2
-- ** Exception: Prelude.Enum.Bool.toEnum: bad argument

----------------------------------------
-- Iso Laws
----------------------------------------

mapList :: Ord k => Iso' (M.Map k v) [(k, v)]
mapList = iso M.toList M.fromList

ex10311 = view (from mapList . mapList) [('z', 3), ('d', 1), ('I', 99)]
-- [('I', 99), ('d', 1), ('z', 3)]

nonEmptyList :: Iso [a] [b] (Maybe (NonEmpty a)) (Maybe (NonEmpty b))
nonEmptyList = iso nonEmpty (maybe [] toList)

-- sorted :: Ord a => Iso' [a] [a]
-- sorted = error "can't implement"

sorted' :: Ord a => Iso' [a] [(Int, a)]
sorted' = iso to' from'
  where
    to' xs = sortOn snd $ zip [0..] xs
    from' xs = fmap snd $ sortOn fst xs

ex10341 = view (sorted' . from sorted') "cab"
-- "cab"
ex10342 = view (from sorted' . sorted') [(9, 'a'), (8, 'b'), (7, 'c')]
-- ex10342 = [(9, 'a'), (8, 'b'), (7, 'c')] ^. from sorted' . sorted'
-- [(2, 'a'), (1, 'b'), (0, 'c')]
