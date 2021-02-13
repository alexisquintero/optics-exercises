{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter3.Exercises where

import Control.Lens
import Control.Lens.Unsound
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Chapter3.Repl

-- Optic Anatomy

e1 :: Integer
e1 = view (_1 . _2) ((1, 2), 3)
-- 2
-- action:    view
-- path:      (_1 . _2)
-- structure: ((1, 2), 3)
-- focus:     2

e2 :: (Bool, Either String c)
e2 = set (_2 . _Left) "new" (False, Left "old")
-- (False, Left "new")
-- action:    set
-- path:      (_2 . _Left)
-- structure: (False, Left "old")
-- focus:     "old"

e3 :: String
e3 = over (taking 2 worded . traversed) toUpper "testing one two three"
-- "TESTING ONE tow three"
-- action:    over
-- path:      (taking 2 worded . traversed)
-- structure: "testing one two three"
-- focus:     "testing one"

e4 = foldOf (both . each) (["super", "cali"], ["fragilistic", "expialidocious"])
-- "supercalifragilisticexpialidocious"
-- action:    foldOf
-- path:      (both . each)
-- structure: (["super", "cali"], ["fragilistic", "expialidocious"])
-- focus:     "super", "cali", "fragilistic", "expialidocious"

----------------------------------------
-- Lens Actions
----------------------------------------

n1 :: Lens' (Bool, (Int, String)) Int
n1 = error ""
-- structure: (Bool, (Int, String))
-- Focus:     Int

n2 :: Lens' (Char, Int) Char
n2 = error ""

-- n3: view, set, over

-- n4: view _3

n5 :: (Bool, Int)
n5 = over _2 (*10) (False, 2)
-- (False, 20)
-- over
---- general:     Lens' s a -> (a -> a) -> s -> s
---- specialized: Lens' (Bool, Int) Int -> (Int -> Int) -> (Bool, Int) -> (Bool, Int)
-- _2
---- general:     Lens' (a, b) a
---- specialized: Lens' (Bool, Int) Int
-- (*10)
-- Int -> Int
-- (False, 2)
-- (Bool, Int)

----------------------------------------
-- Records Part One
----------------------------------------

-- 1: structure <-> s ; focus <-> a
-- 2: getter & setter
-- 3

setName :: Ship -> String -> Ship
setName ship newName = ship { _name = newName }

name :: Lens' Ship String
name = lens _name setName

----------------------------------------
-- Records Part Two
----------------------------------------

-- 1: wand :: Lens' Inventory Wand
--    book :: Lens' Inventory Book
--    potions :: Lens' Inventory [Potion]

-- 2: gazork :: Lens' Chumble Spuzz

-- 3:

data Pet = Pet
  { _petName :: String
  , _petType :: String
  }

makeLenses ''Pet

getPetName :: Pet -> String
getPetName pet = view petName pet

-- makeLenses ''Pet

----------------------------------------
-- Is it a Lens?
----------------------------------------

-- 1
second :: Lens' (a, b, c) b
second = lens getter setter
  where getter (_, x, _) = x
        setter (x1, x2, x3) y2 = (x1, y2, x3)

-- 2
inMaybe :: Lens' (Maybe a) a
inMaybe = lens getter setter
  where getter Nothing = error "No a"
        getter (Just x) = x
        setter ma na = (const na) <$> ma

-- 3
left :: Lens' (Either a b) a
left = lens getter setter
  where getter (Right x) = error "No a"
        getter (Left x) = x
        setter eab na = Left na

-- 4
listThird :: Lens' [a] a
listThird = lens getter setter
  where getter (x : _ : _ : _) = x
        getter _ = error "No a"
        setter (x : x2 : x3 : xs) na = (na : x2 : x3 : xs)

-- 5
conditional :: Lens' (Bool, a, a) a
conditional = lens getter setter
  where getter (True, x, _) = x
        getter (_, _, x) = x
        setter (True, _, e) na = (True, na, e)
        setter (_, e, _) na = (False, e, na)

-- 6
data Err =
    ReallyBadError { _msg :: String }
  | ExitCode       { _code :: Int }
  deriving (Eq)

-- msg :: Lens' Err String
-- msg = lens getter setter
--   where getter ReallyBadError { _msg = m } = m
--         getter ExitCode {} = error "No msg"
--         -- getter ExitCode {} = ""
--         setter _ nm = ReallyBadError nm

msg :: Lens' Err String
msg = lens getMsg setMsg
  where getMsg (ReallyBadError message) = message
        getMsg (ExitCode _) = ""
        setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
        setMsg (ExitCode n) _ = ExitCode n

-- Law 1 (set-get)
newMessage = "False alarm!"
setGet1 :: String
setGet1 = view msg (set msg newMessage (ReallyBadError "BAD BAD BAD"))

setGetLaw1 = newMessage == setGet1
-- True

setGet2 :: String
setGet2 = view msg (set msg newMessage (ExitCode 1))

setGetLaw2 = newMessage == setGet2
-- False

----------------------------------------
-- Laws
----------------------------------------

-- 1

recorder :: Lens' ([a], a) a
recorder = lens getter setter
  where getter (_, a) = a
        setter (history, a) newVal = (a : history, newVal)

-- 2

s1 = ReallyBadError "s1"
s2 = ExitCode 1
getSetMsg s = set msg (view msg s) s

getSetLaw1 = s1 == (getSetMsg s1)
-- True
getSetLaw2 = s2 == (getSetMsg s2)
-- True

msg1 = "msg 1"
msg2 = "msg 2"
setSetMsg1 = set msg msg2 (set msg msg1 s1)
setSetMsg2 = set msg msg2 s1
setSetMsg3 = set msg msg2 (set msg msg1 s2)
setSetMsg4 = set msg msg2 s2

setSetLaw1 = setSetMsg1 == setSetMsg2
-- True
setSetLaw2 = setSetMsg3 == setSetMsg4
-- True

-- 3

msg' :: Lens' Err String
msg' = lens getMsg setMsg
  where getMsg (ReallyBadError message) = message
        getMsg (ExitCode _) = ""
        setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
        setMsg (ExitCode n) newMessage = ReallyBadError newMessage

-- Law 1 (set-get)
setGet1' :: String
setGet1' = view msg' (set msg' newMessage (ReallyBadError "BAD BAD BAD"))

setGetLaw1' = newMessage == setGet1'
-- True

setGet2' :: String
setGet2' = view msg' (set msg' newMessage (ExitCode 1))

setGetLaw2' = newMessage == setGet2'
-- True

-- 4

-- 5

flipper :: Lens' (Bool, a, a) a
flipper = lens getter setter
  where getter (True, a, _) = a
        getter (False, _, a) = a
        setter (True, _, b) newVal = (False, newVal, b)
        setter (False, a, _) newVal = (True, a, newVal)

-- 6

data Builder =
  Builder { _context :: [String]
          , _build   :: [String] -> String
          }

builder :: Lens' Builder String
builder = lens getter setter
  where getter (Builder c f) = f c
        setter (Builder c f) newVal = Builder c (const newVal)

stringBuilder :: Builder
stringBuilder = Builder ["a", "b", "c"] concat

b1 = view builder stringBuilder
-- "abc"

newBuilder :: Builder
newBuilder = set builder "abc" stringBuilder

b2 = view builder newBuilder
-- "abc"

b3 = view builder newBuilder { _context = ["d", "e", "f"]}
-- "abc"

builder' :: Lens' Builder String
builder' = lens getter setter
  where getter (Builder c f) = f c
        setter (Builder c f) newVal = Builder c $ \c' ->
                                        if c' == c
                                        then newVal
                                        else f c'

----------------------------------------
-- Virtual Fields
----------------------------------------

data User =
  User { _firstName :: String
       , _lastName :: String
       -- , _username :: String
       , _email :: String
       } deriving (Show)

makeLenses ''User

username :: Lens' User String
username = email

fullName :: Lens' User String
fullName = lens getter setter
  where getter u = view firstName u <> " " <> view lastName u
        setter user nfn =
          let name : last = words nfn
          in set firstName name $ set lastName (unwords last) user

user = User "John" "Cena" "Invisible@example.com"
fullNamev = view fullName user

user2 = set fullName "Doctor of Thuganomics" user

----------------------------------------
-- Virtual Fields
----------------------------------------

data ProducePrices =
  ProducePrices { _limePrice :: Float
                , _lemonPrice :: Float
                }
  deriving Show

-- 1

nonNegative :: Float -> Float
nonNegative = max 0

limePrice :: Lens' ProducePrices Float
limePrice = lens getter setter
  where getter = _limePrice
        setter pp newPrice = pp { _limePrice = nonNegative newPrice }

lemonPrice :: Lens' ProducePrices Float
lemonPrice = lens getter setter
  where getter = _lemonPrice
        setter pp newPrice = pp { _lemonPrice = nonNegative newPrice }

-- 2

nearPrice :: Float -> Float -> Float
nearPrice otherPrice = min (otherPrice + 0.5) . max (otherPrice - 0.5)

limePrice' :: Lens' ProducePrices Float
limePrice' = lens getter setter
  where getter = _limePrice
        setter (ProducePrices _ other) newPrice =
          let newPrice' = nonNegative newPrice
              newOther = nearPrice newPrice' $ other
          in ProducePrices newPrice' newOther

lemonPrice' :: Lens' ProducePrices Float
lemonPrice' = lens getter setter
  where getter = _lemonPrice
        setter (ProducePrices other _) newPrice =
          let newPrice' = nonNegative newPrice
              newOther = nonNegative . nearPrice newPrice' $ other
          in ProducePrices newOther newPrice'

prices = ProducePrices 1.50 1.48
t1 = set limePrice' 2 prices
t2 = set limePrice' 1.8 prices
t3 = set limePrice' 1.63 prices
t4 = set limePrice' (-1.00) prices
