{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}

module Chapter11.Repl where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Data.Tree
import Data.List.Lens (prefixed)
import Data.Foldable (toList)

----------------------------------------
-- 11. Indexed Optics
----------------------------------------

----------------------------------------
-- 11.1 What are indexed optics?
----------------------------------------

-- itraversed :: TraversableWithIndex i t => IndexedTraversal i (t a) (t b) a b

itraversed1 = toListOf itraversed ["Summer", "Fall", "Winter", "Spring"]
-- ["Summer", "Fall", "Winter", "Spring"]
itraversed2 = itoListOf itraversed ["Summer", "Fall", "Winter", "Spring"]
-- [(0, "Summer"), (1, "Fall"), (2, "Winter"), (3, "Spring")]

-- itoListOf :: IndexedFold i s a -> s -> [(i, a)]
-- (^@..)    :: s -> IndexedFold i s a -> [(i, a)]

agenda = M.fromList [("Monday", "Shopping"), ("Tuesday", "Swimming")]

itolistof1 = agenda ^@.. itraversed
-- [("Monday", "Shopping"), ("Tuesday", "Swimming")]
itolistof2 = (True, "value") ^@.. itraversed
-- [(True, "value")]
t = Node "top" [Node "left" [], Node "right" []]
itolistof3 = t ^@.. itraversed
-- [([] "top"), ([0], "left"), ([1], "right")]

----------------------------------------
-- 11.2 Index composition
----------------------------------------

agenda' :: M.Map String [String] =
  M.fromList [ ("Monady", ["Shopping", "Yoga"])
             , ("Saturday", ["Brunch", "Food coma"])
             ]

itolistof4 = agenda' ^@.. itraversed . itraversed
-- [ (0, "Shopping")
-- , (1, "Yoga")
-- , (0, "Brunch")
-- , (1, "Food coma")
-- ]
-- itolistof5 = agenda' ^@.. itraversed . traverse
-- error: ...
itolistof6 = agenda' ^@.. itraversed <. traverse
-- [ ("Monday", "Shopping")
-- , ("Monday", "Yoga")
-- , ("Saturday", "Brunch")
-- , ("Saturday", "Food coma")
-- ]
itolistof7 = agenda' ^@.. itraversed <.> itraversed
-- [ (("Monday", 0), "Shopping")
-- , (("Monday", 1), "Yoga")
-- , (("Saturday", 0), "Brunch")
-- , (("Saturday", 1), "Food coma")
-- ]
itolistof8 = take 8 $ agenda' ^@.. itraversed <.> itraversed <.> itraversed
-- [ (("Monday", (0, 0)), 'S')
-- , (("Monday", (0, 1)), 'h')
-- , (("Monday", (0, 2)), 'o')
-- , (("Monday", (0, 3)), 'p')
-- , (("Monday", (0, 4)), 'p')
-- , (("Monday", (0, 5)), 'i')
-- , (("Monday", (0, 6)), 'n')
-- , (("Monday", (0, 7)), 'g')
-- ]

----------------------------------------
-- Custom index composition
----------------------------------------

-- icompose :: (i -> j -> k)
--          -> IndexedOptic i s t a b
--          -> IndexedOptic j a b c d
--          -> IndexedOptic k s t c d

-- icompose :: Indexable p c
--          => (i -> j -> p)
--          -> (Indexed i s t -> r)
--          -> (Indexed j a b -> s -> t)
--          -> c a b
--          -> r

showDayAndNumber :: String -> Int -> String
showDayAndNumber a b = a <> ": " <> show b

icompomse1 = agenda' ^@.. icompose showDayAndNumber itraversed itraversed
-- [ ("Monday: 0", "Shopping")
-- , ("Monday: 1", "Yoga")
-- , ("Saturday: 0", "Brunch")
-- , ("Saturday: 1", "Food coma")
-- ]

(.++) :: (Indexed String s t -> r)
      -> (Indexed String a b -> s -> t)
      -> Indexed String a b -> r
(.++) = icompose (\a b -> a ++ ", " ++ b)

populationMap :: M.Map String (M.Map String Int)
populationMap =
  M.fromList
    [ ("Canada", M.fromList [("Ottawa", 994837), ("Toronto", 2930000)])
    , ("Germany", M.fromList [("Berlin", 3748000), ("Munich", 1456000)])
    ]

-- icompose2 = populationMap ^@.. itraversed .++ itraverse
-- [ ("Canada, Ottawa" , 994837)
-- , ("Canada, Toronto" , 2930000)
-- , ("Germany, Berlin" , 3748000)
-- , ("Germany, Munich" , 1456000)
-- ]

-- (<symbols>) :: (Indexed <indexTypeA>  s t -> r)
--             -> (Indexed <indexTypeB>  a b -> s -> t)
--             -> Indexed <combinedType> a b -> r
-- (.++) = icompose <combinationFunction>

----------------------------------------
-- 11.3 Filtering by index
----------------------------------------

-- indices :: (Indexable i p, Applicative f)
--         => (i -> Bool) -> Optical' p (Indexed i) f a a

indices1 = ['a'..'z'] ^.. itraversed . indices even
-- "acegikmoqsuwy"
ratings = M.fromList
  [ ("Dark Knight", 94)
  , ("Dark Knight Rises", 87)
  , ("Death of Superman", 92)
  ]

indices2 = ratings ^.. itraversed . indices (has (prefixed "Dark"))
-- [ 94
-- , 87
-- ]

-- index :: (Indexable i p, Eq i, Applicative f)
--       => i -> Optical' p (Indexed i) f a a

index1 = ['a', 'z'] ^? itraversed . index 10
-- Just 'k'
index2 = ratings ^? itraversed . index "Death of Superman"
-- Just 92

----------------------------------------
-- 11.4 Custom indexed optics
----------------------------------------

data Board a =
  Board
    a a a
    a a a
    a a a
  deriving (Show, Foldable)

data Position = I | II | III
  deriving (Show, Eq, Ord)

testBoard :: Board Char
testBoard =
  Board
    'X' 'O' 'X'
    '.' 'X' 'O'
    '.' 'O' 'X'

----------------------------------------
-- Custom IndexedFolds
----------------------------------------

-- ifolding :: Foldable f => (s -> f (i, a)) -> IndexedFold i s a
-- ifolding ::(Foldable f, Indexable i p, Contravariant g, Applicative g)
--          => (s -> f (i, a))
--          -> Over p g s t a b

slotsFold :: IndexedFold (Position, Position) (Board a) a
slotsFold =
  ifolding $ \board ->
    zip [(x, y) | y <- [I, II, III], x <- [I, II, III]]
        (toList board)

slotsfold1 = testBoard ^@.. slotsFold
-- [ ((I,I) , 'X')
-- , ((II,I) , 'O')
-- , ((III,I) , 'X')
-- , ((I,II) , '.')
-- , ((II,II) , 'X')
-- , ((III,II) , 'O')
-- , ((I,III) , '.')
-- , ((II,III) , 'O')
-- , ((III,III), 'X')
-- ]

slotsfold2 = testBoard ^@.. slotsFold . indices ((== I) . snd)
-- [ ((I , II), '.')
-- , ((II , II), 'X')
-- , ((III, II), 'O')
-- ]

----------------------------------------
-- Custom IndexedTraversals
----------------------------------------

slotsTraversal :: IndexedTraversal (Position, Position) (Board a) (Board b) a b
slotsTraversal p (Board
                   a1 b1 c1
                   a2 b2 c2
                   a3 b3 c3)
  = Board <$> indexed p (I, I)     a1
          <*> indexed p (II, I)    b1
          <*> indexed p (III, I)   c1
          <*> indexed p (I, II)    a2
          <*> indexed p (II, II)   b2
          <*> indexed p (III, II)  c2
          <*> indexed p (I, III)   a3
          <*> indexed p (II, III)  b3
          <*> indexed p (III, III) c3

slotstraversal1 = testBoard & slotsTraversal . indices ((== II) . snd) .~ 'O'
-- Board
--   'X' 'O' 'X'
--   'O' 'O' 'O'
--   '.' 'O' 'X'

printBoard :: Board Char -> IO ()
printBoard = itraverseOf_ slotsTraversal printSlot
  where
    printSlot (III, _) c = putStrLn [c]
    printSlot (_, _) c = putStr [c]

-- printBoard testBoard
-- XOX
-- .XO
-- .OX

-- type IndexedTraversal i s t a b =
--   forall p f . (Indexable i p, Applicative f) => p a (f b) -> s -> f t
--
-- indexed :: Indexable i p => p a b -> i -> a -> b
--
-- ilens :: (s -> (i, a)) -> (s -> b -> t) -> IndexedLens i s t a b

----------------------------------------
-- Index helpers
----------------------------------------

-- indexing :: Traversal s t a b -> IndexedTraversal Int s t a b
-- indexing :: Lens s t a b      -> IndexedLens Int s t a b
-- indexing :: Fold s t a b      -> IndexedFold Int s a
-- indexing :: Getter s a        -> IndexedGetteer Int s a

indexing1 = ("hello" :: T.Text) ^@.. indexing each
-- [(0, 'h'), (1, 'e'), (2, 'l'), (3, 'l'), (4, 'o')]

-- reindexed :: Indexable j p => (i -> j) -> (Indexed i a b -> r) p a b -> r

reindexed1 = ['a'..'c'] ^@.. itraversed
-- [(0, 'a'), (1, 'b'), (2, 'c')]
reindexed2 = ['a'..'c'] ^@.. reindexed (*10) itraversed
-- [(0, 'a'), (10, 'b'), (20, 'c')]
reindexed3 = ['a'..'c'] ^@.. reindexed show itraversed
-- [("0", 'a'), ("1", 'b'), ("2", 'c')]

-- selfIndex :: Indexable a p => p a fb -> a -> fb

selfindex1 = [("Betty", 37), ("Veronica", 12)]
               ^@.. itraversed . selfIndex <. _2
-- [(("Betty", 37), 37), (("Veronica", 12), 12)]

----------------------------------------
-- Index-preserving optics
----------------------------------------

-- preservingindex1 = [('a', True), ('b', False), ('c', True)]
--                    ^@.. itraversed . _1
-- error


-- _1' = cloneIndexPreservingLens _1


-- preservingindex2 = [('a', True), ('b', False), ('c', True)]
--                    ^@.. itraversed . _1'
-- [(0, 'a'), (1, 'b'), (2, 'c')]

-- clonePreservingLens :: Lens s t a b
--                     -> IndexPreservingLens s t a b
-- clonePreservingTraversal :: Lens s t a b
--                          -> IndexPreservingTraversal s t a b
-- clonePreservingSetter :: Lens s t a b
--                       -> IndexPreservingSetter s t a b

-- iplens :: (s -> a) -> (s -> b -> t) -> IndexPreservingLens s t a b
