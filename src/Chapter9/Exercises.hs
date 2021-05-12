{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter9.Exercises where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Control.Monad (guard)

----------------------------------------
-- Prisms
----------------------------------------

data ContactInfo =
    Email String
  | Telephone Int
  | Address String String String

makePrisms ''ContactInfo

-- ex9111 = _Email | _Telephone | _Address

-- ex9121 = Right 35 & _ +~ 5
ex9121 = Right 35 & _Right +~ 5
-- Right 40
-- ex9122 = [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"]
--            ^.. folded . _
ex9122 = [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"]
           ^.. folded . _Just
-- [ "Mind", "Power", "Soul", "Time" ]
-- ex9123 = [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"]
--            & _ <>~ " Stone"
ex9123 = [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"]
           & traversed . _Just <>~ " Stone"
-- [ Just "Mind Stone"
-- , Just "Power Stone"
-- , Nothing
-- , Just "Soul Stone"
-- , Nothing
-- , Just "Time Stone"
-- ]
-- ex9124 = Left (Right True, "Eureka!")
--            & _ %~ not
ex9124 = Left (Right True, "Eureka!")
           & _Left . _1 . _Right %~ not
-- Left (Right False, "Eureka!")
-- ex9125 = _Cons _ ("Do", ["Re", "Mi"])
ex9125 = _Cons # ("Do", ["Re", "Mi"])
-- [ "Do", "Re", "Mi" ]
-- ex9126 = isn't (_Show :: _) "not an int"
ex9126 = isn't (_Show :: Prism' String Int) "not an int"
-- True

input1 = (Just 1, Nothing, Just 3)
ex9131 = input1 ^.. each . _Just
-- [1, 3]
input2 = ('x', "yz")
-- ex9132 = _Cons # input2 :: String
ex9132 = input2
          & review _Cons
          & _tail %~ reverse
-- "xzy"
input3 = "do the hokey pokey"
-- ex9133 = _Left . _Just . _Right # input3
ex9133 :: Either (Maybe (Either Int String)) Bool =
  _Left . _Just . _Right # input3
-- Left (Just (Right "do the hokey pokey"))

----------------------------------------
-- Custom Prisms
----------------------------------------

-- _Tail :: Prism' [a] [a]
-- _Tail = prism' embed match
--   where
--     embed :: a -> [a]
--     embed x = [x]
--     match :: [a] -> Maybe a
--     match [] = Nothing
--     match (h: _) = Just h

_ListCons :: Prism [a] [b] (a, [a]) (b, [b])
_ListCons = prism embed match
  where
    embed :: (b, [b]) -> [b]
    embed (b, bs) = b : bs
    match :: [a] -> Either [b] (a, [a])
    match [] = Left []
    match (a : as) = Right (a, as)

_Cycles :: (Eq a) => Int -> Prism' [a] [a]
_Cycles n = prism' embed match
  where
    match xs
      = let iteration = take (length xs `div` n) xs
        in if (concat $ replicate n iteration) == xs
           then Just iteration
           else Nothing
    embed xs = concat $ replicate n xs

cycles1 = "dogdogdog" ^? _Cycles 3
-- Just "dog"
cycles2 = "dogdogdogdog" ^? _Cycles 3
-- Nothing
cycles3 = "aaa" ^? _Cycles 3
-- Just "a"
cycles4 = "xyz" ^? _Cycles 3
-- Nothing
cycles5 = _Cycles 3 # "dog"
-- "dogdogdog"
cycles6 = "dogdogdog" & _Cycles 3 .~ "cats"
-- "catscatscats"

----------------------------------------
-- Prisms Laws
----------------------------------------

_Contains :: forall a . Ord a => a -> Prism' (S.Set a) (S.Set a)
-- _Contains x = prism' (S.insert x) match
_Contains x = prism' embed match
  where
    -- match xs = S.filter (/= x) xs <$ guard (S.member x xs)
    embed :: S.Set a -> S.Set a
    embed = S.insert x
    match :: (S.Set a) -> Maybe (S.Set a)
    match sa = if S.member x sa
               then Just (S.delete x sa)
               else Nothing

contains1 = S.fromList [1, 2, 3] ^? _Contains 2
-- Just (fromList [1, 3])
contains2 = S.fromList [1, 2, 3] ^? _Contains 10
-- Nothing
contains3 = _Contains 10 # S.fromList [1, 2, 3]
-- fromList [1, 2, 3, 10]
contains4 = _Contains 2 # S.fromList [1, 2, 3]
-- fromList [1, 2, 3]

-- Law one
ex9311 = preview (_Contains 1) (review (_Contains 1) (S.singleton 1))
-- Just (fromList [])

_Singleton :: forall a . Prism' [a] a
_Singleton = prism' embed match
  where
    match :: [a] -> Maybe a
    match [a] = Just a
    match _ = Nothing
    embed :: a -> [a]
    embed a = [a]

-- Law one
ex9321 = preview _Singleton (review _Singleton 1)
-- Just 1
-- Law two
ex9322 = preview (_Singleton :: Prism' [Int] Int) ([1] :: [Int])
-- [1]
ex9323 = review _Singleton 1
-- [1]
