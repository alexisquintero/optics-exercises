{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter4.Repl where

import Control.Lens
import Control.Lens.Unsound
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

----------------------------------------
-- 4.1 Introduction to polymorphic optics
----------------------------------------

data Promotion a =
  Promotion { _item :: a
            , _discountProcentage :: Double
            }
  deriving (Show)

item :: Lens (Promotion a) (Promotion b) a b
item = lens getter setter
  where getter :: Promotion a -> a
        getter = _item
        setter :: Promotion a -> b -> Promotion b
        setter pa b = pa { _item = b }

peachPromo = Promotion "A really delicious Peach" 25.0
buffyFigurines = ["Buffy", "Angel", "Willow", "Giles"]
buffyPromo = set item buffyFigurines peachPromo

data Preferences a =
  Preferences { _best :: a
              , _worst :: a
              }
  deriving (Show)

best :: Lens (Preferences a) (Preferences b) a b
best = lens getter setter
  where getter :: (Preferences a) -> a
        getter = _best
        setter :: (Preferences a) -> b -> (Preferences b)
        -- setter pa b = pa { _best = b }
        setter pa b = error "Type mismatch"

----------------------------------------
-- 4.3 Composing Lenses
----------------------------------------

data Person =
  Person { _name :: String
         , _address :: Address
         }
  deriving (Show)

data Address =
  Address { _streetAddress :: StreetAddress
          , _city :: String
          , _country :: String }
  deriving (Show)

data StreetAddress =
  StreetAddress { _streetNumber :: String
                , _streetName :: String
                }
  deriving (Show)

makeLenses ''Person
makeLenses ''Address
makeLenses ''StreetAddress

sherlock :: Person
sherlock =
  Person { _name = "S. Holmes"
         , _address = Address
           { _streetAddress = StreetAddress
             { _streetNumber = "221B"
             , _streetName = "Baker Street"
             }
             , _city = "London"
               , _country = "England"
           }
  }

setStreetNumber :: String -> Person -> Person
setStreetNumber newStreetAddress person =
  let existingAddress = _address person
      existingStreetAddress = _streetAddress existingAddress
  in person { _address = existingAddress
              { _streetAddress = existingStreetAddress
                { _streetNumber = newStreetAddress
                }
              }
            }

personNoLens = setStreetNumber "221A" sherlock

updateAddress :: (Address -> Address) -> (Person -> Person)
updateAddress modify existingPerson =
  existingPerson { _address = modify . _address $ existingPerson }

updateStreetAddress :: (StreetAddress -> StreetAddress) -> (Address -> Address)
updateStreetAddress modify existingAddress =
  existingAddress { _streetAddress = modify . _streetAddress $ existingAddress }

updateStreetNumber :: (String -> String) -> (StreetAddress -> StreetAddress)
updateStreetNumber modify existingStreetAddress =
  existingStreetAddress {
    _streetNumber = modify . _streetNumber $ existingStreetAddress
  }

-- modifier :: (a -> a)
-- updater  :: (a -> a) -> (s -> s)

personModifier =
  (updateAddress . updateStreetAddress . updateStreetNumber)
    (const "221A")
    sherlock

updaters ::              (String ->   String) -> Person ->   Person
lenses   :: Functor f => (String -> f String) -> Person -> f Person
-- lenses :: Lens' Person String
updaters = (updateAddress . updateStreetAddress . updateStreetNumber)
lenses   = (address       . streetAddress       . streetNumber)

data Player = Player deriving Show
data Wool = Wool deriving Show
data Sweater = Sweater deriving Show

data Item a =
  Item { _material :: a
       , _amount :: Int
       }
  deriving Show

makeLenses ''Item

-- material :: Lens  (Item a) (item b) a b
-- amount   :: Lens' (Item a) a

weave :: Wool -> Sweater
weave Wool = Sweater
gameState :: (Player, Item Wool)
gameState = (Player, Item Wool 5)

newState = over (_2 . material) weave gameState
-- (Player, Item { _material = Sweater, _amount = 5 })

-- (_2 . material) :: Lens (other, Item a) (other, Item b) a b
-- (_2 . material) :: Lens (Player, Item Wool) (Player, Item Sweater) Wool Sweater
