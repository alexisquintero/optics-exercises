{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Chapter16.Repl where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Control.Lens.Plated
import Data.Data

import Data.Tree
import Data.Tree.Lens (branches, root)

import Data.Data.Lens

----------------------------------------
-- 16. Uniplate - Manipulating recursive data
----------------------------------------

----------------------------------------
-- 16.1 A Brief History
----------------------------------------

----------------------------------------
-- 16.2 Control.Lens.Plated
----------------------------------------

-- class Plated a where
--   plate :: Traversal' a a
--
-- instance Plated [a] where
--   plate handler (x:xs) = (x:) <$> handler xs
--   plate _ [] = pure []
--
-- instance Plated (Tree a) where
--   plate handler (Node a as) = Node a <$> traverse handler as

data Expr =
    Val Int
  | Expr :+: Expr
  | Expr :*: Expr
  deriving (Show, Data, Eq)

deriving instance Plated Expr

----------------------------------------
-- Children
----------------------------------------

-- children :: Plated a => a -> [a]

children1 = children [1, 2, 3]
-- [[2, 3]]
children2 = children ((Val 1 :+: (Val 2 :*: Val 3)) :*: Val 4)
-- [ Val 1 :+: (Val 2 :*: Val 3)
-- , Val 4

----------------------------------------
-- Rewrite
----------------------------------------

-- rewrite :: Plated a => (a -> Maybe a) -> a -> a

upDup :: Eq a => [a] -> Maybe [a]
upDup (x:y:xs) | x == y = Just (x : xs)
upDup _ = Nothing

rewrite1 = rewrite upDup [1, 2, 2, 2, 3, 2, 4]
-- [1, 2, 3, 2, 4]

-- Remove additions and multiplications that have no effect
simplifyAST :: Expr -> Maybe Expr
simplifyAST (Val 0 :+: e) = Just e
simplifyAST (e :+: Val 0) = Just e
simplifyAST (Val 1 :*: e) = Just e
simplifyAST (e :*: Val 1) = Just e
simplifyAST _  = Nothing

rewrite2 = rewrite simplifyAST ((Val 1 :*: Val 10) :+: Val 0)
-- Val 10

----------------------------------------
-- Universe
----------------------------------------

-- universe :: Plated a => a -> [a]

universe1 = universe [1, 2, 3, 4]
-- [[1, 2, 3, 4], [2, 3, 4], [3, 4], [4], []]

universe2 = universe (Val 1 :*: (Val 2 :+: Val 3))
-- [ Val 1 :*: (Val 2 :+: Val 3)
-- , Val 1
-- , Val 2 :+: Val 3
-- , Val 2
-- , Val 3
-- ]

cosmos1 = toListOf cosmos (Val 1 :*: (Val 2 :+: Val 3))
-- [ Val 1 :*: (Val 2 :+: Val 3)
-- , Val 1
-- , Val 2 :+: Val 3
-- , Val 2
-- , Val 3
-- ]

----------------------------------------
-- Transform
----------------------------------------

-- transform :: Plated a => (a -> a) -> a -> a

swap :: Expr -> Expr
swap (a :+: b) = a :*: b
swap (a :*: b) = a :+: b
swap e = e

transform1 = transform swap ((Val 1 :*: Val 2) :+: Val 3)
-- (Val 1 :+: Val 2 :*: Val 3

----------------------------------------
-- Deep
----------------------------------------

-- deep :: (Conjoined p, Applicative f, Plated s)
--      => Traversing p f s s a b -> Over p f s s a b
--
-- deep :: Plated s => Fold s a          -> Fold s a
-- deep :: Plated s => Traversal s s a b -> Traversal s s a b

t :: Tree (M.Map String String)
t = Node (M.singleton "junk" "ignored")
      [ Node (M.singleton "treasure" "gems")
          [ Node (M.singleton "treasure" "rubies") [] ]
      , Node (M.fromList [ ("treasure", "gold") ] ) []
      ]

deep1 = t ^.. deep (root . ix "treasure")
-- [ "gems", "gold" ]
deep2 = t & deep (root . ix "treasure") .~ "fool's gold"
-- Node (M.singleton "junk" "ignored")
--   [ Node (M.singleton "treasure" "fool's gold")
--     [Node (M.singleton "treasure" "rubies") []]
--   , Node (M.fromList [("treasure", "fool's gold")]) []
-- ]

notdeep = t ^.. cosmos . root . ix "treasure"
-- [ "gems", ,"rubies", "gold" ]

----------------------------------------
-- 16.3 Overriding `plate`
----------------------------------------

-- uniplate :: Data a => Traversal' a a

searching :: (a -> Bool) -> Traversal' (Tree a) (Tree a)
searching predicate = branches . traversed . filtered (predicate . rootLabel)

tree :: Tree Int
tree = Node 10
         [ Node 4
             [ Node 2 [], Node 3[]]
         , Node 7
             [ Node 8 [], Node 3[]]
         ]

searching1 = tree ^.. cosmosOf (searching (>5)) . root
-- [10, 7, 8]
searching2 = tree ^.. cosmosOf (searching odd) . root
-- [10, 7, 3]
searching3 = tree ^.. cosmosOf (searching even) . root
-- [10, 4, 2]
searching4 = transformOf (searching even) (root %~ negate) tree
-- Node (-10)
--   [ Node (-4)
--       [ Node (-2) [], Node 3 []]
--   , Node 7
--       [ Node 8 [], Node 3[]]
--   ]

----------------------------------------
-- 16.4 The magic of `biplate`
----------------------------------------

data Employee =
  Employee { _name :: String
           , _salary :: Int
           } deriving (Show, Data)

data Company =
  Company { _humanResources :: [Employee]
          , _salesPeople    :: [Employee]
          , _managers       :: [Employee]
          } deriving (Show, Data)

makeLenses ''Employee
makeLenses ''Company

dunderMifflin :: Company
dunderMifflin =
  Company { _humanResources = [Employee "Toby Flenderson" 40000]
          , _salesPeople    = [Employee "Dwight Schrute"  60000]
          , _managers       = [Employee "Michael Scott"   80000]
          }

updateSalaryBoring :: Company -> Company
updateSalaryBoring c =
  c & humanResources . traversed . salary +~ 1000
    & salesPeople    . traversed . salary +~ 1000
    & managers       . traversed . salary +~ 1000

updatesalary1 = updateSalaryBoring dunderMifflin
-- ...
updatesalary2 = dunderMifflin & biplate +~ (1000 :: Int)
-- ...

data Employee2 =
  Employee2 { _name' :: String
            , _salary' :: Integer
            } deriving (Show, Data)

updatesalary3 = dunderMifflin & biplate +~ (1000 :: Int)
-- dunderMifflin ...

data Employee3 =
  Employee3 { _name'' :: String
            , _salary'' :: Int
            , _age :: Int
            } deriving (Show, Data)

updatesalary4 = dunderMifflin & biplate +~ (1000 :: Int)
-- age updated too

----------------------------------------
-- 16.5 Exercises - Uniplate
----------------------------------------

-- TODO
