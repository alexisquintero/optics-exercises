{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter10.Repl where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Data.Char (toUpper, isSpace)
import Numeric.Lens
import Data.List (intercalate)
import Data.Coerce (coerce)

----------------------------------------
-- 10. Isos
----------------------------------------

----------------------------------------
-- 10.1 Introduction to Isos
----------------------------------------

----------------------------------------
-- How do Isos fit into the hierarchy?
----------------------------------------

----------------------------------------
-- There and back again
----------------------------------------

to' :: String -> T.Text
to' = T.pack

from' :: T.Text -> String
from' = T.unpack

-- T.pack . T.unpack = id
-- t.unpack . T.pack = id

-- to . from = id
-- from . to = id

----------------------------------------
-- 10.2 Building Isos
----------------------------------------

-- iso :: (s -> a) -> (b -> t) -> Iso s t a b

packed :: Iso' String T.Text
packed = iso to' from'
  where
    to' :: String -> T.Text
    to' = T.pack
    from' :: T.Text -> String
    from' = T.unpack

packed1 = "Ay, caramba!" ^. packed
-- "Ay, caramba!" :: T.Text
packed2 = packed # "Sufferin' Succotash"
-- "Sufferin' Succotash" :: String

----------------------------------------
-- 10.3 Flipping isos with `from`
----------------------------------------

-- from :: Iso s t a b -> Iso b a t s
-- from :: Uso' s a -> Iso' a s

from1 = ("Good grief" :: String) ^. packed
-- "Good grief" :: T.Text
from2 = ("Good grief" :: T.Text) ^. from packed
-- "Good grief" :: String

unpacked :: Iso' T.Text String
unpacked = from packed

----------------------------------------
-- 10.4 Modification under isomorphism
----------------------------------------

str = "Idol on a pedestal" :: String
packed3 = str & packed %~ T.replace "Idol" "Sand"
-- "Sand on a pedestal" :: String
packed4 = over packed (T.replace "Idol" "Sand") str
-- "Sand on a pedestal" :: String
txt = "Lorem ipsum" :: T.Text
packed5 = txt & from packed . traversed %~ toUpper
-- "LOREM IPSUM"

----------------------------------------
-- 10.5 Varieties of Isomorphisms
----------------------------------------

-- reverse :: [a] -> [a]
-- reversed :: Iso' [a] [a]
-- reversed = iso reverse reverse

-- involuted :: (a -> a) -> Iso' a a
-- involuted f = iso f f

-- reversed' :: Iso' [a] [a]
-- reversed' = involuted reverse

reverse1 = reverse . reverse $ [1, 2, 3]
-- [1, 2, 3]
reversed1 = [1, 2, 3] & reversed %~ drop 1
-- [1, 2]
reversed2 = [1, 2, 3] & reversed %~ take 1
-- [3]
reversed3 = [1, 2, 3, 4] ^.. reversed . takingWhile (> 2) traversed
-- [4, 3]
reversed4 = "Blue suede shoes" & reversed . taking 1 worded . reversed .~ "gloves"
-- "Blue suede gloves"

-- swapped :: Iso (s, s') (t, t') (a, a') (b, b')
-- swapped :: Iso' (a, b) (b, a)

swapped1 = ("Fall", "Pride") ^. swapped
-- ("Pride", "Fall")

-- swapped :: (Bifunctor p, Swapped p) => Iso (p a b) (p c d) (p b a) (p d c)

swapped2 = Right "Field" ^. swapped
-- Left "Field"

-- flipped :: Iso' (a -> b -> c) (b -> a -> c)

(++?) = (++) ^. flipped
flipped1 = "A" ++? "B"
-- "BA"

-- curried   :: Iso' ((a b) -> c) (a -> b -> c)
-- uncurried :: Iso' (a -> b -> c) ((a, b) -> c)

addTuple = (+) ^. uncurried
uncurried1 = addTuple (1, 2)
-- 3

negated1 = 10 ^. negated
-- -10
negated2 = over negated (+10) 30
-- 20
adding1 = 100 ^. adding 50
-- 150
dividing1 = 100.0 ^. dividing 10
-- 10.0

----------------------------------------
-- Composing isos
----------------------------------------

txt1 = "Winter is coming" :: T.Text
unpacked1 = txt1 ^. unpacked . reversed
-- "gnimoc si retniW"
unpacked2 = txt1 & unpacked . reversed %~ takeWhile (not . isSpace)
-- "coming" :: T.Text

dividing2 = 30 & dividing 10 . multiplying 2 +~ 1
-- 35

----------------------------------------
-- 10.6 Projecting Isos
----------------------------------------

-- mapping' :: Functor f => Iso' s a -> Iso' (f s) (f a)
-- mapping' i = is (fmap (view i)) (fmap (review i))

-- mapping :: (Functor f, Functor g)
--         => Iso s t a b -> Iso (f s) (g t) (f a) (g b)

toYamlList :: [String] -> String
toYamlList xs = "- " <> intercalate "\n- " xs

shoppingLists = ["Milk", "Eggs", "Flour"] :: [String]
shoppingListt = ["Milk", "Eggs", "Flour"] :: [T.Text]

toYamlList1 = toYamlList shoppingLists
-- - Milk
-- - Eggs
-- - Flour

strShoppingList = shoppingListt ^. mapping unpacked :: [String]
toYamlList2 = toYamlList strShoppingList
-- - Milk
-- - Eggs
-- - Flour
toYamlList3 = shoppingListt ^. mapping unpacked . to toYamlList
-- - Milk
-- - Eggs
-- - Flour
-- toYamlList4 = traverseOf_ (mapping unpacked . to toYamlList) putStrLn $ shoppingLists
-- - Milk
-- - Eggs
-- - Flour

-- contramapping :: Contravariant f
--               => Iso s t a b -> Iso (f a) (f b) (f s) (f t)
-- contramapping :: Contravariant f
--               => Iso' s a -> Iso (f a) (f s)

-- bimapping :: (Bifunctor f Bifunctor g)
--           => Iso s t a b -> Iso s' t' a' b'
--           -> Iso (f s s') (g t t') (f a a') (g b b')
-- bimapping :: Bifunctor f
--           => Iso' s a -> Iso' s' a'
--           -> Iso' (f s s') (f a a')

-- dimapping :: (Profunctor p, Profunctor q)
--           => Iso s t a b -> Iso s' t' a' b'
--           -> Iso (p a s') (q b t') (p s a') (q t b')
-- dimapping :: Profunctor p
--           => Iso' s a -> Iso' s' a'
--           -> Iso' (p a s') (p s a')

textToYamlList :: [T.Text] -> T.Text
textToYamlList = toYamlList ^. dimapping (mapping unpacked) packed
-- textToYamlList = T.pack . toYamlList . fmap T.unpack

----------------------------------------
-- 10.7 Isos and newtypes
----------------------------------------

----------------------------------------
-- Coercing with isos
----------------------------------------

-- coerce :: Coercible a b => a -> b

newtype Email = Email String
  deriving (Show)

newtype UserID = UserID String
  deriving (Show)

-- :info Email
-- newtype Email = Email String
-- instance Show Email

coerce1 = coerce ("joe@example.com" :: String) :: Email
-- Email "joe@example.com"
coerce2 = coerce (Email "joe@example.com") :: UserID
-- UserID "joe@example.com"

-- coerced :: (Coercible s a, Coercible t b) => Iso s t a b

cerced1 = over coerced
            (reverse :: String -> String)
            (Email "joe@example.com") :: Email
-- Email "moc.elpamxe@eol"

-- coerced2 = Email "joe@example.com" & coerced .traversed %~ toUpper
coerced2 = Email "joe@example.com"
            & (coerced :: Iso' Email String) .traversed %~ toUpper
-- Email "JOE@EXAMPLE.COM"

email :: Iso' Email String
email = coerced

coerced3 = Email "joe@example.com" & email . traversed %~ toUpper
-- Email "JOE@EXAMPLE.COM"

newtype Email' = Email' {_email' :: String}
  deriving Show
makeLenses ''Email'

-- :browse Chapter10.Repl
-- newtype Email' = Email' {_email' :: String}
-- email' :: Iso' Email String
-- newtype UserID = UserID String

----------------------------------------
-- Newtype wrappers isos
----------------------------------------

-- _Wrapped   :: Wrapped :: s => Iso' s (Unwrapped s)
-- _Unwrapped :: Wrapped :: s => Iso' (Unwrapped s) s

-- makeWrapped ''Email'

-- wrapped1 = Email' "joe@example.com" & _Wrapped %~ reverse
-- Email {_email = "moc.elpamxe@eol"}

----------------------------------------
-- 10.8 Laws
----------------------------------------

----------------------------------------
-- The one and only law: Reversibility
----------------------------------------

-- myIso . from myIso == id
-- from myIso . myIso == id

law1 = view (reversed . from reversed) ("Testing one two three" :: String)
-- "Testing one two three"
law2 = view (from reversed . reversed) ("Testing one two three" :: String)
-- "Testing one two three"
law3 = view (negated . from negated) 23
-- 23
law4 = view (from negated . negated) 23
-- 23

myIso :: Iso' Double Double
myIso = negated . adding 10.0 . multiplying 372.0

law5 = view (myIso . from myIso) 23.0
-- 23.0
law6 = view (from myIso . myIso) 23.0
-- 23.00000000000000227
