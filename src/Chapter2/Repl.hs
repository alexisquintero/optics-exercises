{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter2.Repl where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Numeric.Lens (negated)
import Data.Data.Lens (biplate)
import Data.Data
import Data.List (sort)
import Data.Bits.Lens (bitAt)

-- 2.4

-- ex1 :: String
-- ex1 = view (address . country) person

-- "Canada"

ex2 :: (Char, Char, Bool)
ex2 = set _3 False ('a', 'b', 'c')

-- ('a', 'b', False)

ex3 :: Int
ex3 = sumOf (folded . _2 . _Left)
        [(True, Left 10), (False, Right "pepperoni"), (True, Left 20)]

-- 30

stories :: [String]
stories = ["This one time at band camp", "Nuff said.", "This is a short story"]

ex4 :: [String]
ex4 = over
        (traversed . filtered ((>10) . length))
        (\story -> take 10 story ++ "...")
        stories

-- ["This one t...", "Nuff said.", This is a ..."]

-- 2.5

ex5 :: Int
ex5 = sumOf (folded . beside negated id) [Left 1, Right 10, Left 2, Right 20]

-- 27

-- ex6 :: (Num n, Data n) => (Maybe n, Either ([Char], [n]) n)
-- ex6 = (Just 3, Left ("hello", [13, 15, 17])) & biplate *~ 100

-- (Just 300, Left ("hello", [1300, 1500, 1700]))

ex7 :: [Integer]
ex7 = [1, 2, 3, 4, 5, 6, 7, 8] & partsOf (traversed . filtered even) %~ sort

-- [1, 8, 3, 6, 5, 4, 7, 2]

ex8 :: [Integer]
ex8 = [1, 2, 3, 4] & traversed . bitAt 1 %~ not

-- [3, 0, 1, 6]

prompts :: (String, String, String)
prompts = ( "What is your name?"
          , "What is your quest?"
          , "What is your favourite color?"
          )

ex9 :: IO(String, String, String)
ex9 = prompts & each %%~ (\prompt -> putStrLn prompt >> getLine)

-- ("Sir Galahad", "To seek the holy grail", "Blue I think?")
