{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter6.Repl where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Data.Char (toUpper)
import Data.Ord (comparing)
import Control.Monad.State
import Data.Monoid

----------------------------------------
-- 6.1 Introduction to Folds
----------------------------------------

----------------------------------------
-- Focusing all elements of a container
----------------------------------------

data Role =
    Gunner
  | PowderMonkey
  | Navigator
  | Captain
  | FirstMate
  deriving (Show, Eq, Ord)

data CrewMember =
  CrewMember { _name :: String
             , _role :: Role
             , _talents :: [String]
             }
  deriving (Show, Eq, Ord)

makeLenses ''CrewMember

roster :: S.Set CrewMember
roster = S.fromList
  --           Name               Role         Talents
  [ CrewMember "Grumpy Roger"     Gunner       ["Jugling", "Arbitrage"]
  , CrewMember "Long-John Bronze" PowderMonkey ["Origami"]
  , CrewMember "Salty Steve"      PowderMonkey ["Charcuterie"]
  , CrewMember "One-eyed Jack"    Navigator    []
  ]

myFold :: Fold s a
myFold = error ""

-- rosterRoles :: Fold (S.Set CrewMember) Role

toListSomehow :: Fold (S.Set CrewMember) Role -> S.Set CrewMember -> [Role]
toListSomehow _ _ = error ""

crewMembers :: Fold (S.Set CrewMember) CrewMember
crewMembers = error ""

----------------------------------------
-- Collecting focuses as a list
----------------------------------------

-- foldView1 = roster ^. crewMembers
foldView2 = ["a", "b", "c"] ^. folded

tlo1 = toListOf folded roster
tlo2 = roster ^.. folded

justFold = Just "Buried Treasure" ^.. folded
-- [ "Buried Treasure" ]
nothingFold = Nothing ^.. folded
-- []
idenFold = Identity "Cutlass" ^.. folded
-- [ "Cutlass" ]
tupleFold = ("Rubles", "Gold") ^.. folded
-- [ "Gold" ]
mapFold = M.fromList [("Jack", "Captain"), ("Will", "First Mate")]
  ^.. folded
-- [ "Captain", "First Mate" ]

----------------------------------------
-- Using lenses as folds
----------------------------------------

jerry = CrewMember "Jerry" PowderMonkey ["Ice Cream Making"]
jerryRole = jerry ^.. role

----------------------------------------
-- Composing folds
----------------------------------------

roles = roster ^.. folded . role

-- folded :: Fold (S.Set CrewMember) CrewMember
-- role :: Fold CrewMember Role

----------------------------------------
-- Foundational fold combinators
----------------------------------------

bitTuple = ("Gemini", "Leo") ^.. both
-- ["Gemini", "Leo"]
bitLeft = Left "Albuquerque" ^.. both
-- ["Albuquerque"]
bitRight = Right "Yosemite" ^.. both
-- ["Yosemite"]
bit3Tuple = ("Gemini", "Leo", "Libra") ^.. both
-- ["Leo", "Libra"]

eachTuple = (1, 2, 3, 4, 5) ^.. each
-- [1, 2, 3, 4, 5]
listEach = [1, 2, 3, 4, 5] ^.. each
-- [1, 2, 3, 4, 5]
-- textEach = ("Made him an offer he couldn't refuse" :: T.Text)
textEach = "Made him an offer he couldn't refuse"
  ^.. each
-- "Made him an offer he couldn't refuse"

-- byteStringEach = ("Do or do not" :: BS.ByteString) ^.. each
-- [68, 111, 32, 111, 114, 32, 100, 111, 32, 110, 111, 116]

----------------------------------------
-- 6.2 Custom Folds
----------------------------------------

-- folding :: Foldable f => (s -> f a) -> Fold s a

newtype Name = Name
  { getName :: String
  } deriving (Show)

data ShipCrew = ShipCrew
  { _shipName :: Name
  , _captain :: Name
  , _firstMate :: Name
  , _conscripts :: [Name]
  } deriving (Show)

makeLenses ''ShipCrew

collectCrewMembers :: ShipCrew -> [Name]
collectCrewMembers crew =
  [_captain crew, _firstMate crew] ++ _conscripts crew

crewMembers' :: Fold ShipCrew Name
crewMembers' = folding collectCrewMembers

myCrew :: ShipCrew
myCrew =
  ShipCrew
  { _shipName = Name "Purple Pearl"
  , _captain = Name "Grumpy Roger"
  , _firstMate = Name "Long-John Bronze"
  , _conscripts = [Name "One-eyed Jack", Name "Filthy Frank"]
  }

crew = myCrew ^.. crewMembers'

----------------------------------------
-- Mapping over folds
----------------------------------------

-- to :: (s -> a) -> Fold s a

toGetName = Name "Two-faced Tony" ^. to getName
-- "Two-faced Tony"

toToUpper = Name "Two-faced Tony" ^. to getName . to (fmap toUpper)
-- TWO-FACED TONY"

toComposition = Name "Two-faced Tony" ^. to (fmap toUpper . getName)
-- TWO-FACED TONY"

crew' = myCrew ^.. crewMembers' . to getName

----------------------------------------
-- Combining multiple folds on the same structure
----------------------------------------

crewNames :: Fold ShipCrew Name
crewNames =
  folding (\s -> s ^.. captain
              <> s ^.. firstMate
              <> s ^.. conscripts . folded)

crewNamesString = myCrew ^.. crewNames . to getName

----------------------------------------
-- 6.3 Fold actions
----------------------------------------

elemOf1 = elemOf folded 3 [1, 2, 3, 4]
-- True
elemOf2 = elemOf folded 99 [1, 2, 3, 4]
-- True

anyOf1 = anyOf folded even [1, 2, 3, 4]
-- True
anyOf2 = anyOf folded (>10) [1, 2, 3, 4]
-- False

allOf1 = allOf folded even [1, 2, 3, 4]
-- False
allOf2 = allOf folded (<10) [1, 2, 3, 4]
-- True

findOf1 = findOf folded even [1, 2, 3, 4]
-- Just 2
findOf2 = findOf folded (>10) [1, 2, 3, 4]
-- Nothing

has1 = has folded []
-- False
has2 = has folded [1, 2]
-- True

hasn't1 = hasn't folded []
-- True
hasn't2 = hasn't folded [1, 2]
-- False

lengthOf1 = lengthOf folded [1, 2, 3, 4]
-- 4

sumOf1 = sumOf folded [1, 2, 3, 4]
-- 10

productOf1 = productOf folded [1, 2, 3, 4]
-- 24

firstOf1 = firstOf folded []
-- Nothing
firstOf2 = firstOf folded [1, 2, 3, 4]
-- Just 1
firstOf3 = preview folded [1, 2, 3, 4]
-- Just 1
firstOf4 = [1, 2, 3, 4] ^? folded
-- Just 1

lastOf1 = lastOf folded [1, 2, 3, 4]
-- Just 4

minimumOf1 = minimumOf folded [2, 1, 4, 3]
-- Just 1
maximumOf1 = maximumOf folded [2, 1, 4, 3]
-- Just 4

minimumOf2 :: Ord a => Maybe a
minimumOf2 = minimumOf folded []
-- Nothing

maximumOf2 :: Ord a => Maybe a
maximumOf2 = maximumOf folded []
-- Nothing

----------------------------------------
-- Queries case study
----------------------------------------

data Actor =
  Actor { _name' :: String
        , _birthYear :: Int
        } deriving (Show, Eq)

makeLenses ''Actor

data TVShow =
  TVShow { _title :: String
         , _numEpisodes :: Int
         , _numSeasons :: Int
         , _criticScore :: Double
         , _actors :: [Actor]
         } deriving (Show, Eq)

makeLenses ''TVShow

howIMetYourMother :: TVShow
howIMetYourMother = TVShow
  { _title = "How I Met Your Mother"
  , _numEpisodes = 208
  , _numSeasons = 9
  , _criticScore = 83
  , _actors =
    [ Actor "Josh Radnor" 1974
    , Actor "Cobie Smulders" 1982
    , Actor "Neil Patrick Harris" 1973
    , Actor "Alyson Hanningan" 1974
    , Actor "Jason Segel" 1980
    ]
  }

buffy :: TVShow
buffy = TVShow
  { _title = "Buffy the Vampire Slayer"
  , _numEpisodes = 144
  , _numSeasons = 7
  , _criticScore = 81
  , _actors =
    [ Actor "Sarah Michelle Gellar" 1977
    , Actor "Alyson Hannigan" 1974
    , Actor "Nicholas Brendon" 1971
    , Actor "David Boreanaz" 1969
    , Actor "Anthony Head" 1954
    ]
  }

tvShows :: [TVShow]
tvShows = [ howIMetYourMother
          , buffy
          ]

tvShowsSumOfNumEpisodes = sumOf (folded . numEpisodes) tvShows
-- 352

tvShowsMaximumOfCriticScore =
  maximumOf (folded . criticScore) tvShows
-- Just 83.0
tvShowsMaximumOfCriticScoreName =
  _title <$> maximumByOf folded (comparing _criticScore) tvShows
-- Just "How I Met Your Mother"

tvShowsOldestActor =
  minimumByOf
   (folded . actors . folded)
   (comparing _birthYear)
   tvShows
-- Just
--  ( Actor
--      { _name = "Anthony Head"
--      , _birthYear = 1954
--      }
--  )

comparingOf :: Ord a => Lens' s a -> s -> s -> Ordering
-- comparingOf l = comparing (view l)
comparingOf = comparing . view

tvShowsOldestActor' =
  minimumByOf
   (folded . actors . folded)
   (comparingOf birthYear)
   tvShows
-- Just
--  ( Actor
--      { _name = "Anthony Head"
--      , _birthYear = 1954
--      }
--  )

----------------------------------------
-- Folding with effects
----------------------------------------

calcAge :: Actor -> Int
-- calcAge actor = 2030 - _birthYear actor
calcAge = (2030 -) . _birthYear

showActor :: Actor -> String
showActor actor = _name' actor <> ": " <> show (calcAge actor)

traverseOfActor = traverseOf_ ( folded
                              . actors
                              . folded
                              . to showActor)
                              putStrLn
                              tvShows

traverseOfNumActors =
  execState (traverseOf_ folded (modify . const (+1)) tvShows) 0
-- 2

----------------------------------------
-- Combining fold results
----------------------------------------

-- foldOf    :: Monoid a => Fold s a -> s -> a
-- foldMapOf :: Monoid r => s a -> (a -> r) -> s -> r

ageSummary :: Actor -> (Sum Int, Sum Int)
ageSummary actor = (Sum 1, Sum (calcAge actor))

foldOfAgeSummary =
  foldOf (folded . actors . folded . to ageSummary) tvShows
-- (Sum {getSum = 10}, Sum {getSum = 572})

computeAverage :: (Sum Int, Sum Int) -> Double
computeAverage (Sum count, Sum total) =
  fromIntegral total / fromIntegral count

ageAverage = computeAverage foldOfAgeSummary
-- 57.2

foldMapOfAgeAverage =
  computeAverage $ foldMapOf (folded . actors . folded) ageSummary tvShows
-- 57.2

----------------------------------------
-- Using 'view' on folds
----------------------------------------

foldView3 = Just "do it" ^. folded
-- "do it"
-- foldView4 = Just (42 :: Int) ^. folded
foldView5 = Nothing ^. folded :: String
-- ""
foldView6 = ("one", "two", "three") ^. each
-- "onetwothree"

----------------------------------------
-- Customizing monoidal folds
----------------------------------------

actorCount =
  foldMapOf (folded . actors . folded . name')
            (\n -> M.singleton n 1)
            tvShows

-- fromList
--  [ ("Alyson Hannigan",1)
--  , ("Anthony Head",1)
--  , ("Cobie Smulders",1)
--  , ("David Boreanaz",1)
--  , ("Jason Segel",1)
--  , ("Josh Radnor",1)
--  , ("Neil Patrick Harris",1)
--  , ("Nicholas Brendon",1)
--  , ("Sarah Michelle Gellar",1)
--  ]

badMap = M.singleton 'a' "first" <> M.singleton 'a' "second"
-- fromList [('a', "first")]

-- M.unionWith :: Ord k => (a -> a -> a) -> M.Map k a -> M.Map k a -> M.Map k a

goodMap =
  M.unionWith (+) (M.singleton "an actor" 1) (M.singleton "an actor" 1)
-- fromList [("an actor", 2)]

-- foldByOf    :: Fold s a -> (a -> a -> a) -> a -> s  -> a
-- foldMapByOf :: Fold s a -> (r -> r -> r) -> r -> (a -> r) -> s -> r
-- foldrOf     :: Fold s a -> (a -> r -> r) -> r -> s -> r
-- foldlOf     :: Fold s a -> (r -> a -> r) -> r -> s -> r

actorCount2 = foldMapByOf
                (folded . actors . folded . name')
                (M.unionWith (+))
                mempty
                (\n -> M.singleton n 1)
                tvShows
-- fromList
--   [ ("Alyson Hannigan",2)
--   , ("Anthony Head",1)
--   , ("Cobie Smulders",1)
--   , ("David Boreanaz",1)
--   , ("Jason Segel",1)
--   , ("Josh Radnor",1)
--   , ("Neil Patrick Harris",1)
--   , ("Nicholas Brendon",1)
--   , ("Sarah Michelle Gellar",1)
--   ]

actorCount3 = foldByOf
                (folded . actors . folded . name' . to (\n -> M.singleton n 1))
                (M.unionWith (+))
                mempty
                tvShows

actorCount4 = foldrOf
                (folded . actors . folded . name' . to (\n -> M.singleton n 1))
                (\cur acc -> M.unionWith (+) cur acc)
                mempty
                tvShows

actorCount5 = foldlOf
                (folded . actors . folded . name' . to (\n -> M.singleton n 1))
                (\acc cur -> M.unionWith (+) cur acc)
                mempty
                tvShows

----------------------------------------
-- 6.4 Higher Order Folds
----------------------------------------

taking1 = [1, 2, 3, 4] ^.. taking 2 folded
-- [1, 2]
dropping1 = [1, 2, 3, 4] ^.. dropping 2 folded
-- [3, 4]
taking3 = [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. folded . taking 2 folded
-- [1, 2, 10, 20, 100, 200]
taking4 = ("Albus", "Dumbledore") ^.. both . taking 3 folded
-- "AlbDum"
taking5 =
  [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. taking 2 (folded . folded)
-- [1, 2]
taking6 = ("Albus", "Dumbledore") ^.. taking 3 (both . folded)
-- "Alb"
taking7 = ("Albus", "Dumbledore") ^.. taking 3 both . folded
-- "AlbusDumbledore"
taking8 =
  [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. (taking 2 folded) . folded
-- [1, 2, 3, 10, 20, 30]
taking9 =
  (["albus", "dumbledore"], ["severus", "snape"])
    ^.. taking 3 (both . folded)
-- ["albus", "dumbledore", "severus", "snape"]
taking10 =
  (["albus", "dumbledore"], ["severus", "snape"])
    ^.. taking 3 (both . folded) . folded
-- "albusdumbledoreseverussnape"
dropping2 =
  [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. dropping 2 (folded . folded)
-- [3, 10, 20, 30, 100, 200, 300]
dropping3 =
  [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. folded . dropping 2 folded
-- [3, 30, 3000]
dropping4 =
  [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. dropping 2 folded . folded
-- [100, 200, 300]
dropping5 = ("Albus", "Dumbledore") ^.. both . dropping 2 folded
-- "busmbledore"
dropping6 = ("Albus", "Dumbledore") ^.. dropping 2 (both . folded)
-- "busDumbledore"

----------------------------------------
-- Backwards
----------------------------------------

backwards1 = [1, 2, 3] ^.. backwards folded
-- [3, 2, 1]
backwards2 = ("one", "two") ^.. backwards both
-- ["two", "one"]
backwards3 = [(1, 2), (3, 4)] ^.. backwards (folded . both)
-- [4, 3, 2, 1]
backwards4 = [(1, 2), (3, 4)] ^.. backwards folded . both
-- [3, 4, 1, 2]
backwards5 = [(1, 2), (3, 4)] ^.. folded . backwards both
-- [2, 1, 4, 3]

----------------------------------------
-- TakingWhile, DroppingWhile
----------------------------------------

takingWhile1 = [1..100] ^.. takingWhile (<10) folded
-- [1..9]
takingWhile2 = [1, 5, 15, 5, 1] ^.. takingWhile (<10) folded
-- [1, 5]
droppingWhile1 = [1..100] ^.. droppingWhile (<90) folded
-- [90..100]
droppingWhile2 = [1, 5, 15, 5, 1] ^.. droppingWhile (<10) folded
-- [15, 5, 1]

----------------------------------------
-- 6.5 Filtering folds
----------------------------------------

-- filtered :: (s -> Bool) -> Fold s s
-- filtered :: (Choice p, Applicative f) => (a -> Bool) -> Optic' p f a a

filtered1 = [1, 2, 3, 4] ^.. folded . filtered even
-- [2, 4]
filtered2 = ["apple", "passionfruit", "orange", "pomegranate"]
              ^.. folded
                . filtered ((>6) . length)
-- ["passionfruit", "pomegranate"]

data Card =
  Card { _name'' :: String
       , _aura :: Aura
       , _holo :: Bool
       , _moves :: [Move]
       } deriving (Show, Eq)

data Aura
  = Wet
  | Hot
  | Spark
  | Leafy
  deriving (Show, Eq)

data Move =
  Move { _moveName :: String
       , _movePower :: Int
       } deriving (Show, Eq)

makeLenses ''Card
makeLenses ''Move

deck :: [Card]
deck = [ Card "Skwortul" Wet False [Move "Squirt" 20]
       , Card "Scorchander" Hot False [Move "Scorch" 20]
       , Card "Seedasaur" Leafy False [Move "Allergize" 20]
       , Card "Kapichu" Spark False [Move "Poke" 10 , Move "Zap" 30]
       , Card "Elecdude" Spark False [Move "Asplode" 50]
       , Card "Garydose" Wet True [Move "Gary's move" 40]
       , Card "Moisteon" Wet False [Move "Soggy" 3]
       , Card "Grasseon" Leafy False [Move "Leaf Cut" 30]
       , Card "Spicyeon" Hot False [Move "Capsaicisize" 40]
       , Card "Sparkeon" Spark True [Move "Shock" 40 , Move "Battery" 50]
       ]

filtered3 = lengthOf (folded . aura . filtered (==Spark))
                     deck
-- 3
filtered4 = lengthOf (folded . moves . folded . movePower . filtered (>30))
                     deck
-- 5
filtered5 = deck
              ^.. folded
                . filtered (anyOf (moves . folded . movePower) (>40))
                . name''
-- [ "Elecdude", "Sparkdeon" ]
filtered6 = lengthOf (folded . (filtered ((==Spark) . _aura)) . moves .folded)
                     deck
-- 5
filtered7 = deck
              ^.. folded
                . filtered ((== Spark) . _aura)
                . moves
                . folded
                . filtered ((>30) . _movePower)
                . moveName
-- [ "Asplode", "Shock", "Battery" ] 

-- filteredBy :: Fold s a -> Fold s s
-- filteredBy :: Fold s a -> IndexedTraversal' a s s
-- filteredBy :: (Indexable i p, Applicative f)
--               => Getting (First i) a i -> p a (f a) -> a -> f a

filteredBy1 = deck
                ^.. folded
                  . filteredBy (aura . only Spark)
                  . moves
                  . folded
                  . filteredBy (movePower . filtered (>30))
                  . moveName
-- [ "Asplde", "Shock", "Battery" ]

-- only :: Eq a => a -> Fold a ()
-- only :: Eq a => a -> Prism' a ()

only1 = 1 ^? only 1
-- Just ()
only2 = 2 ^? only 1
-- Nothing
only3 = has (only "needle") "needle"
-- True
only4 = has (only "needle") "haystack"
-- False

filtered8 = maximumByOf
              (folded . filtered _holo)
              -- (folded . filteredBy holo)
              (comparing (lengthOf moves))
              deck

-- Just
--   ( Card
--     { _name = "Sparkeon"
--     , _aura = Spark
--     , _holo = True
--     , _moves =
--       [ Move
--         { _moveName = "Shock"
--         , _movePower = 40
--         }
--       , Move
--         { _moveName = "Battery"
--         , _movePower = 50
--         }
--       ]
--     }
--   )
