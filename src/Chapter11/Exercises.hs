{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter11.Exercises where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Data.List as L

----------------------------------------
-- Indexed Optics
----------------------------------------

-- ex11111 = M.fromList [("streamResponse", False), ("useSSL", True)]
--             _ itraversed
ex11111 = M.fromList [("streamResponse", False), ("useSSL", True)]
            ^@.. itraversed
-- [("streamResponse, False"), ("useSSL", True)]
-- ex11112 = ( M.fromList [('a', 1), ('b', 2)]
--           , M.fromList [('c', 3), ('d', 4)])
--             ^@.. _
ex11112 = ( M.fromList [('a', 1), ('b', 2)]
          , M.fromList [('c', 3), ('d', 4)])
            ^@.. both . itraversed
-- [('a', 1), ('b', 2), ('c', 3), ('d', 4)]
-- ex11113 = M.fromList [('a', (True, 1)), ('b', (False, 2))]
--             ^@.. itraversed _ _1
ex11113 = M.fromList [('a', (True, 1)), ('b', (False, 2))]
            ^@.. itraversed <. _1
-- [('a', True), ('b', False)]
-- ex11114 = [ M.fromList [("Tulips", 5), ("Roses", 3)]
--           , M.fromList [("Goldfish", 11), ("Frogs", *)]
--           ] ^@.. _
ex11114 = [ M.fromList [("Tulips", 5), ("Roses", 3)]
          , M.fromList [("Goldfish", 11), ("Frogs", 8)]
          ] ^@.. itraversed <.> itraversed
-- [ ((0,"Roses"), 3)
-- , ((0,"Tulips"), 5)
-- , ((1,"Frogs"), 8)
-- , ((1,"Goldfish"), 11)
-- ]
-- ex11115 = [10, 20, 30] & itraversed _ (+)
ex11115 = [10, 20, 30] & itraversed %@~ (+)
-- [10, 21, 32]
-- ex11116 = _
--             itraversed
--             (\i s -> putStrLn (replicate i ' ' <> s))
--             ["one", "two", "three"]
ex11116 = itraverseOf_
            itraversed
            (\i s -> putStrLn (replicate i ' ' <> s))
            ["one", "two", "three"]
--  one
--    two
--      three
-- ex11117 = itraverseOf_
--             itraversed
--             (\n s -> putStrLn _)
--             ["Go shopping", "Eat lunch", "take a nap"]
ex11117 = itraverseOf_
            itraversed
            (\n s -> putStrLn (show n <> ": " <> s))
            ["Go shopping", "Eat lunch", "take a nap"]
-- 0: Go shopping
-- 1: Eat lunch
-- 2: Take a nap

----------------------------------------
-- Index Filters
----------------------------------------

exercises :: M.Map String (M.Map String Int)
exercises = M.fromList
  [ ("Monday"   , M.fromList [("pushups", 10), ("crunches", 20)])
  , ("Wednesday", M.fromList [("pushups", 15), ("handstands", 3)])
  , ("Friday"   , M.fromList [("crunches", 25), ("handstands", 5)])
  ]

-- ex11211 = exercises ^.. traversed . itraversed . index "crunches"
ex11211 = sumOf (traversed . itraversed . index "crunches") exercises
-- 45
-- ex11212 = exercises ^.. itraversed . index "Wednesday" . traversed
ex11212 = sumOf (itraversed . index "Wednesday" . traversed) exercises
-- 18
ex11213 = exercises ^@.. itraversed <. itraversed . index "pushups"
-- [("Monady", 10), ("Wednesday", 15)]
ex11213' = exercises ^.. traversed . itraversed . index "pushups"
-- [10, 15]

board :: [String] = [ "XOO"
                    , ".XO"
                    , "X.."
                    ]

ex11221 = board ^@.. itraversed <.> itraversed
-- [ ((0,0),'X') ,((0,1),'O') ,((0,2),'O')
-- , ((1,0),'.') ,((1,1),'X') ,((1,2),'O')
-- , ((2,0),'X') ,((2,1),'.') ,((2,2),'.')
-- ]
ex11222 = board & itraversed . index 1 . itraversed . index 0 .~ 'X'
ex11222' = board & (itraversed <.> itraversed) . index (1, 0) .~ 'X'
-- [ "XOO"
-- , "XXO"
-- , "X.."]
ex11223 = (L.transpose board) ^. itraversed . index 1
ex11223' = board ^.. traversed . itraversed . index 1
-- "OX."
ex11224 = board ^. itraversed . index 2
ex11224' = board ^.. (itraversed <. traverse) . index 2
-- "X.."

----------------------------------------
-- Custom Indexed Optics
----------------------------------------

pair :: IndexedFold Bool (a, a) a
pair =
  ifolding $ \(p1, p2) ->
    zip [(odd x) | x <- [0..]]
        [p1, p2]

pair1 = ('a', 'b') ^@.. pair
-- [(False, 'a'), (True, 'b')]

pair' :: IndexedTraversal Bool (a, a) (b, b) a b
pair' p (p1, p2) = (,) <$> indexed p False p1
                       <*> indexed p True p2

pair'1 = ('a', 'b') ^@.. pair'

oneIndexed :: IndexedTraversal Int [a] [b] a b
oneIndexed = reindexed succ itraversed
-- oneIndexed = reindexed succ traversed

oneindexed1 = ['a'..'d'] ^@.. oneIndexed
-- [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd')]

invertedIndex :: IndexedTraversal Int [a] [b] a b
invertedIndex =
  reindexed
    (\(xs, i) -> (length xs - 1) - i)
    (selfIndex <.> traversed)

invertedindex1 = ['a'..'d'] ^@.. invertedIndex
-- [(3, 'a'), (2, 'b'), (1, 'c'), (0, 'd')]

chars :: IndexedTraversal Int T.Text T.Text Char Char
chars = indexing each

chars1 = ("banana" :: T.Text) ^@.. chars
-- [(0,'b'),(1,'a'),(2,'n'),(3,'a'),(4,'n'),(5,'a')]

charCoords :: IndexedTraversal (Int, Int) String String Char Char
charCoords = indexing lined <.> indexing each

charcoords1 = "line\nby\nline" ^@.. charCoords
-- [ ((0,0),'l'),((0,1),'i'),((0,2),'n'),((0,3),'e')
-- , ((1,0),'b'),((1,1),'y')
-- , ((2,0),'l') ,((2,1),'i'),((2,2),'n'),((2,3),'e')]
