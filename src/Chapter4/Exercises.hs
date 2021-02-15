{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter4.Exercises where

import Control.Lens
import Control.Lens.Unsound
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Chapter4.Repl

----------------------------------------
-- Polymorphic Lenses
----------------------------------------

-- 1

type Vorpal a = [a]

vorpal :: Lens (Vorpal x) (Vorpal y) x y
vorpal = error ""

-- 2

favourites :: Lens (Preferences a) (Preferences b) (a, a) (b, b)
favourites = error ""

-- or

data Preferences' a b =
  Preferences' { _best' :: a
               , _worst' :: b
               }
  deriving (Show)

best'' :: Lens (Preferences' a b) (Preferences' a' b) a a'
best'' = error ""
worst' :: Lens (Preferences' a b) (Preferences' a b') b b'
worst' = error ""

-- or

best' :: Lens (Preferences a) (Preferences b) a b
best' = lens getter setter
  where getter = _best
        setter pa b = pa { _best = b, _worst = b }

worst :: Lens (Preferences a) (Preferences b) a b
worst = lens getter setter
  where getter = _worst
        setter pa b = pa { _best = b, _worst = b }

-- 3

data Result e =
  Result { _lineNumber :: Int
         , _result :: Either e String
         }
  deriving (Show)

result :: Lens (Result e) (Result f) (Either e String) (Either f String)
result = error ""

-- 4

data ParseResult e a =
    Error e
  | Result' a
  deriving (Show)

result' :: Lens (ParseResult e a) (ParseResult f b) (Either e a) (Either f b)
result' = lens getter setter
  where getter (Error e) = Left e
        getter (Result' a) = Right a
        setter _ (Left e) = Error e
        setter _ (Right a) = Result' a

-- 5

data Predicate a =
  Predicate (a -> Bool)

predicate :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
predicate = lens getter setter
  where getter :: (Predicate a) -> (a -> Bool)
        getter (Predicate f) = f
        setter :: (Predicate a) -> (b -> Bool) -> (Predicate b)
        setter (Predicate f) b = Predicate b
        -- setter :: (Predicate a) -> (b -> a) -> (Predicate b)
        -- setter (Predicate f) b = Predicate (b . f)

----------------------------------------
-- Lens Composition
----------------------------------------

-- 1

names = ("Ginerva", (("Galileo", "Waldo"), "Malfoy"))
-- view _ names
-- "Waldo"
waldo = view (_2 . _1 . _2) names

-- 2

data Five = Five
data Eight = Eight
data Two = Two
data Three = Three

fiveEightDomino :: Lens' Five Eight
fiveEightDomino = error ""
-- mysteryDomino   :: Lens' ???? ?????
twoThreeDomino  :: Lens' Two Three
twoThreeDomino = error ""

mysteryDomino :: Lens' Eight Two
mysteryDomino = error ""

dominoTrain :: Lens' Five Three
dominoTrain = fiveEightDomino . mysteryDomino . twoThreeDomino

-- 3

data Armadillo = Armadillo
data Hedgehog = Hedgehog
data Platypus = Platypus
data BabySloth = BabySloth

animalsLens :: Functor f => (Armadillo -> f Hedgehog) -> (Platypus -> f BabySloth)
animalsLens = error ""

animalsLens' :: Lens Platypus BabySloth Armadillo Hedgehog
animalsLens' = error ""

-- Pre-action structure: Platypus
-- Post-action structure: BabySloth
-- Pre-action focus: Armadillo
-- Post-action focus: Hedgehog

-- 4

data Chumble = Chumble
data Spuzz = Spuzz
data Gazork = Gazork
data Trowlg = Trowlg
data Bandersnatch = Bandersnatch
data Yakka = Yakka
data Zink = Zink
data Wattoom = Wattoom
data Grug = Grug
data Pubbawup = Pubbawup
data Foob = Foob
data Mog = Mog
data Boojum = Boojum
data Jabberwock = Jabberwock
data Snark = Snark
data JubJub = JubJub

spuzorktrowmble  :: Lens Chumble      Spuzz      Gazork       Trowlg
gazorlglsnatchka :: Lens Gazork       Trowlg     Bandersnatch Yakka
zinkattumblezz   :: Lens Zink         Wattoom    Chumble      Spuzz
gruggazinkoom    :: Lens Grug         Pubbawup   Zink         Wattoom
banderyakoobog   :: Lens Bandersnatch Yakka      Foob         Mog
boowockugwup     :: Lens Boojum       Jabberwock Grug         Pubbawup
snajubjumwock    :: Lens Snark        JubJub     Boojum       Jabberwock
spuzorktrowmble = error ""
gazorlglsnatchka = error ""
zinkattumblezz = error ""
gruggazinkoom = error ""
banderyakoobog = error ""
boowockugwup = error ""
snajubjumwock = error ""

-- compAll :: Functor f => (Foob -> f Mog) -> (Snark -> f JubJub)
compAll :: Lens Snark JubJub Foob Mog
compAll =
  snajubjumwock .
  boowockugwup .
  gruggazinkoom .
  zinkattumblezz .
  spuzorktrowmble .
  gazorlglsnatchka .
  banderyakoobog
