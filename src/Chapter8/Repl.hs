{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter8.Repl where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Data.ByteString as BS
import Data.Tree
import Data.Maybe (fromMaybe)

----------------------------------------
-- 8. Indexable Structures
----------------------------------------

----------------------------------------
-- 8.1 What's an "indexable" structure?
----------------------------------------

xs = ["Henry I", "Henry II", "Henry III"] 
xs0 = xs !! 0
-- Henry I
xs1 = xs !! 1
-- Henry II
xs2 = xs !! 3
-- *** Exception: Prelude.!!: index too large

turtles = M.fromList [("Leo", "Katanas"), ("Raph", "Sai")]
turtles1 = M.lookup "Leo" turtles
-- Just "Katanas"
turtles2 = M.adjust ("Two " <>) "Leo" turtles
-- fromList [("Leo", "Two Katanas"), ("Raph", "Sai")]

----------------------------------------
-- 8.2 Accessing and updating values with 'Ixed'
----------------------------------------

----------------------------------------
-- The `ixed` Class
----------------------------------------

-- class Ixed m where
--   ex :: Index m -> Traversal' m (IxValue m)

-- type instance Index [a] = Int
-- type instance Index (Map k a) = k
-- type instance IxValue [a] = a
-- type instance IxValue (Map k a) = a
-- type instance Index ByteString = Int
-- type instance Index Text = Int
-- type instance IxValue Text = Char
-- type instance IxValue ByteString = Word8

----------------------------------------
-- Accessing and setting values with `ix`
----------------------------------------

humanoids = ["Borg", "Cardassian", "Talaxian"]
ix1 = humanoids ^? ix 1
-- Just "Cardassian"
ix2 = humanoids ^? ix 10
-- Nothing
ix3 = humanoids & ix 10 .~ "Romulan"
-- ["Borg", "Cardassian", "Talaxian"]
benders = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]
ix4 = benders ^? ix "Zuko"
-- Just "Fire"
ix5 = benders ^? ix "Sokka"
-- Nothing
ix6 = benders & ix "Toph" .~ "Metal"
-- M.fromList [("Katara", "Water"), ("Toph", "Metal"), ("Zuko", "Fire")]
ix7 = benders & ix "Iroh" .~ "Lightning"
-- M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]

----------------------------------------
-- Indexed Structures
----------------------------------------

----------------------------------------
-- Indexing monomorphic types
----------------------------------------

ix8 = ("hello" :: T.Text) ^? ix 0
-- Just 'h'
ix9 = ("hello" :: T.Text) ^? ix 10
-- Nothing
ix10 = ("hello" :: T.Text) & ix 0 .~ 'j'
-- "jello"
ix11 = ("hello" :: BS.ByteString) ^? ix 0
-- Just 104
ix12 = ("hello" :: BS.ByteString) ^? ix 10
-- Nothing
ix13 = ("hello" :: BS.ByteString) & ix 0 +~2
-- "jello"
ix14 = ("hello" :: T.Text) & ix 1 %%~ \_ -> ("aeiou" :: [Char])
-- [ "hallo"
-- , "hello"
-- , "hillo"
-- , "hollo"
-- , "hullo"
-- ]

----------------------------------------
-- Indexing stranger structures
----------------------------------------

tree :: Tree Int
tree = Node 1 [ Node 2 [ Node 4 []]
              , Node 3 [ Node 5 [], Node 6 []]]

ix15 = tree ^? ix []
-- Just 1
ix16 = tree ^? ix [0]
-- Just 2
ix17 = tree ^? ix [0, 0]
-- Just 4
ix18 = tree ^? ix [1, 0]
-- Just 5
ix19 = tree ^? ix [1, 1]
-- Just 6
ix20 = tree ^? ix [5, 6]
-- Nothing

ix21 = reverse ^? ix "Stella!"
-- Just "!alletS"
specialReverse = reverse & ix "password" .~ "You found the secret!"
ix22 = specialReverse "password"
-- "You found the secret!"
ix23 = specialReverse "Stella!"
-- "!alletS"

----------------------------------------
-- 8.3 Inserting & Deleting with `At`
----------------------------------------

----------------------------------------
-- Map-like structures
----------------------------------------

turtles3 = M.insert "Mikey" "Nunchaku" turtles
-- M.fromList [("Leo", "Katanas"), ("Mikey", "Nunchaku"), ("Raph", "Sai")]
turtles4 = M.delete "Leo" turtles
-- M.fromList [("Raph", "Sai")]

-- class At where
--   at :: Index m -> Lens' m (Maybe (IxValue m))

-- ix :: Index m -> Traversal' m (IxValue m)
-- at :: Index m -> Lens'      m (Maybe (IxValue m))

at1 = benders ^. at "Zuko"
-- Just "Fire"
at2 = benders & at "Zuko" .~ Nothing
-- M.fromList [("Katara", "Water"), ("Toph", "Earth")]
at3 = benders & at "Iroh" .~ Just "Lightning"
-- M.fromList [ ("Iroh", "Lightning")
--            , ("Katara", "Water")
--            , ("Toph", "Earth")
--            , ("Zuko", "Fire")
--            ]

-- (?~) :: Traversal s t a (Maybe b) -> b -> s -> t

at4 = benders & at "Iroh" ?~ "Lightning"
-- M.fromList [ ("Iroh", "Lightning")
--            , ("Katara", "Water")
--            , ("Toph", "Earth")
--            , ("Zuko", "Fire")
--            ]

optLens1 = (1, 2) & both ?~ "twins!"
-- (Just "twins~", Just "twins~")

-- sans :: At m => Index m -> m -> m
-- sans k = at k .~ Nothing

sans1 = sans "Katara" benders
-- M.fromList [("Toph", "Earth"), ("Zuko", "Fire")]

----------------------------------------
-- Manipulating Sets
----------------------------------------

primes :: S.Set Int
primes = S.fromList [2, 3, 5, 7, 11, 13]

ix24 = primes ^? ix 5
-- Just ()
ix25 = primes ^? ix 4
-- Nothing
at5 = primes & at 17 ?~ ()
-- S.fromList [2, 3, 5, 7, 11, 13, 17]
sans2 = sans 5 primes
-- S.fromList [2, 3, 7, 11, 13]
sans3 = primes & sans 5
               & sans 7
               & sans 11
-- S.fromList [2, 3, 13]

----------------------------------------
-- 8.4 Custom Indexed Data Structures
----------------------------------------

----------------------------------------
-- Custom Ixed: Cyclical indexing
----------------------------------------

xs' = Cycled ['a', 'b', 'c']

cycled1 = xs' ^? ix 1
-- Just 'b'
cycled2 = xs' ^? ix 3
-- Just 'a'
cycled3 = xs' ^? ix 10
-- Just 'b'
cycled4 = xs' ^? ix (-1)
-- Just 'b'
cycled5 = xs' & ix 0 .~ '!'
-- Cycled "!bc"
cycled6 = xs' & ix 10 .~ '!'
-- Cycled "a!c"
cycled7 = xs' & ix (-1) .~ '!'
-- Cycled "ab!"

newtype Cycled a = Cycled [a]
  deriving Show

type instance Index (Cycled a) = Int
type instance IxValue (Cycled a) = a

-- instance Ixed (Cycled a) where
--   ix :: Int -> Traversal' (Cycled a) a
--   ix i handler (Cycled xs) =
--     Cycled <$> (traverseOf (ix (i `mod` length xs)) handler xs)

-- ix :: Int -> Traversal' (Cycled a) a
-- ix :: Applicative f => Int -> (a -> f a) -> Cycled a -> f (Cycled a)

instance Ixed (Cycled a) where
  ix :: Applicative f => Int -> (a -> f a) -> Cycled a -> f (Cycled a)
  ix i handler (Cycled xs) =
    Cycled <$> (traverseOf (ix (i `mod` length xs)) handler xs)

----------------------------------------
-- Custom At: Address indexing
----------------------------------------

data Address =
  Address { _buildingNumber :: Maybe String
          , _streetName :: Maybe String
          , _apartmentNumber :: Maybe String
          , _postalCode :: Maybe String
          } deriving Show

makeLenses ''Address

-- type instance Index Address = ??
-- type instance IxValue Address = String

data AddressPiece
  = BuildingNumber
  | StreetName
  | ApartmentNumber
  | PostalCode
  deriving Show

type instance Index Address = AddressPiece
type instance IxValue Address = String

instance Ixed Address

instance At Address where
  at :: AddressPiece -> Lens' Address (Maybe String)
  at BuildingNumber = buildingNumber
  at StreetName = streetName
  at ApartmentNumber = apartmentNumber
  at PostalCode = postalCode

addr = Address Nothing Nothing Nothing Nothing
-- Address { _buildingNumber = Nothing
--         , _streetName = Nothing
--         , _apartmentNumber = Nothing
--         , _postalCode = Nothing
--         }
sherlockAddr = addr & at StreetName ?~ "Baker St."
                    & at ApartmentNumber ?~ "221B"
-- Address { _buildingNumber = Nothing
--         , _streetName = Just "Baker St."
--         , _apartmentNumber = Just "221B"
--         , _postalCode = Nothing
--         }
sherlockAddr2 = sherlockAddr & ix ApartmentNumber .~ "221A"
-- Address { _buildingNumber = Nothing
--         , _streetName = Just "Baker St."
--         , _apartmentNumber = Just "221A"
--         , _postalCode = Nothing
--         }
sherlockAddr3 = sherlockAddr & sans StreetName
-- Address { _buildingNumber = Nothing
--         , _streetName = Nothing
--         , _apartmentNumber = Just "221A"
--         , _postalCode = Nothing
--         }

----------------------------------------
-- 8.5 Handling missing values
----------------------------------------

-- failover :: Alternative m => LensLike ((,) Any) s t a b -> (a -> b_ -> s -> m t
-- failover :: Traversal s t a b -> (a -> b) -> s -> Maybe t

failover1 = "abcd" & failover (ix 6) toUpper :: Maybe String
-- Nothing
failover2 = "abcd" & failover (ix 2) toUpper :: Maybe String
-- Just "abCd"
failover3 = "abcd" & failover (ix 6) toUpper :: IO String
-- *** Exception: user error (mzero)
failover4 = [] & failover _head (*10) :: Maybe [Int]
-- Nothing
failover5 = (M.fromList[('a', [1, 2])]
              & failover (ix 'a' . ix 3) (* 10) :: Maybe (M.Map Char [Int]))
-- Nothing
failover6 = [1, 3, 5] & failover ( traversed . filtered even) (* 10) :: Maybe [Int]
-- Nothing
failover7 = [1, 4, 5] & failover (traversed . filtered even) (* 10) :: Maybe[Int]
-- Just [1, 40, 5]
-- s1 :: String = "abcdefg"
-- failover8 :: String = failover (ix 8) toUpper s1
--                         <|> failover (ix 6) toUpper s1
--                         <|> failover (ix 4) toUpper s1
-- "abcdefG"

----------------------------------------
-- Fallbacks with 'failing'
----------------------------------------

-- failing :: (Conjoined p, Applicative f)
--         => Traversing p f s t a b
--         -> Over p f s t a b
--         -> Over p f s t a b
--
-- failing :: Fold s t a b -> Fold s t a b -> Fold s t a b
-- failing :: Traversal s t a b -> Traversal s t a b -> Traversal s t a b

-- failing1 = M.fromList [('a', 1), ('b', 2)]
--              ^? (ix 'z' `faling` ix 'b')
-- Just 2
failing2 = M.fromList [('a', 1), ('b', 2)]
             & (ix 'z' `failing` ix 'b') *~ 10
-- fromList [('a', 1), ('b', 20)]
failing3 = M.fromList [("Bieber" :: String, "Believe"),
                       ("Beyoncé", "Lemonade")]
             ^? (ix "Swift" `failing` ix "Bieber" `failing` ix "Beyoncé")
-- Just "Belive"
failing4 = M.fromList [('a', (1, [2, 3, 4])), ('b', (5, [6, 7, 8]))]
             ^.. (ix 'z' . _1
                  `failing` ix 'a' . _2 . ix 10
                  `failing` ix 'b' . _2 . traversed
                  )
-- [6, 7, 8]

----------------------------------------
-- Default elements
----------------------------------------

-- non :: Eq a => a -> Iso' (Maybe a) a
-- non :: Eq a => a -> Traversal' (Maybe a)

non1 = Nothing ^. non 0
-- 0
non2 = Nothing ^. non "default"
-- "default"
non3 = Just "value" ^. non "default"
-- "value"
favouriteFoods =
  M.fromList [("Garfield", "Lasagna"), ("Leslie", "Waffles")]
non4 = favouriteFoods ^. at "Leslie" . non "Pizza"
-- "Waffles"
non5 = favouriteFoods ^. at "Leo" . non "Pizza"
-- "Pizza"
non6 = favouriteFoods & at "Popeye" . non "Pizza" .~ "Spinach"
-- fromList [ ("Garfield", "Lasagna")
--          , ("Leslie"  , "Waffles")
--          , ("Popeye"  , "Spinach")
--          ]

fav name = at name . non "Pizza"
newFavourites = favouriteFoods & fav "Garfield" .~ "Pizza"
-- fromList [("Leslie", "Waffles")]
non7 = newFavourites ^. fav "Garfield"
-- "Pizza"
non8 = M.fromList [("Jim", 32), ("Dwight", 39)]
         & at "Erin" . non 0 +~ 10
-- M.fromList [("Dwight", 39), ("Erin", 10), ("Jim", 32)]
non9 = M.fromList [("Jim", 32), ("Dwight", 39)]
         & at "Jim" . non 0 +~ 8
-- M.fromList [("Dwight", 39), ("Jim", 40)]
non10 = M.fromList [("Jim", 32), ("Dwight", 39)]
          & at "Dwight" . non 0 .~ 0
-- M.fromList [("Jim", 40)]

----------------------------------------
-- Checking fold success/failure
----------------------------------------

fromMaybe1 = fromMaybe 'z' (("abc" :: String) ^? ix 10)
-- 'z'

-- pre :: Getting (First a) s a -> IndexPreservingGetter s (Maybe a)
-- pre :: Fold s a -> Getter s (Maybe a)

pre1 = ("abc" :: String) ^. pre (ix 10) . non 'z'
-- 'z'
pre2 = [1, 2, 3, 4] ^. pre (traversed . filtered even)
-- Just 2
pre3 = [1, 3] ^. pre (traversed . filtered even)
-- Nothing
pre4 = ("abc" :: String) ^. pre (ix 20) . non 'z'
-- 'z'
