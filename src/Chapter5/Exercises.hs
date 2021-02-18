{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter5.Exercises where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Data.Char (toUpper)

-- 1

data Gate =
  Gate { _open :: Bool
       , _oilTemp :: Float
       }
  deriving (Show)

makeLenses ''Gate

data Army =
  Army { _archers :: Int
       , _knights :: Int
       }
  deriving (Show)

makeLenses ''Army

data Kingdom =
  Kingdom { _name :: String
          , _army :: Army
          , _gate :: Gate
          }
  deriving (Show)

makeLenses ''Kingdom

duloc :: Kingdom
duloc =
  Kingdom { _name = "Duloc"
          , _army = Army { _archers = 22
                         , _knights = 14
                         }
          , _gate = Gate { _open = True
                         , _oilTemp = 10.0
                         }
          }

goalA = duloc
  & name <>~ ": a perfect place"
  & army . knights +~ 28
  -- & army . knights *~ 3
  & gate . open &&~ False

goalB = duloc
  & name <>~ "instein"
  & army . archers -~ 5
  & army . knights +~ 12
  & gate . oilTemp ^~ 2

goalC = duloc
  & name <>~ ": Home"
  & gate . oilTemp //~ 2
  & name <<<>~ " of the talking Donkeys"
  -- & name <<>~ ": Home"
  -- & _2 . name <>~ " of the talking Donkeys"
  -- & _2 . gate . oilTemp //~ 2

-- 2

-- (False, "opposums") `undefied` _1 ||~ True
ex21 = (False, "opposums") & _1 ||~ True
-- (True, "opposums")

-- 2 & id `undefined` 3
ex22 = 2 & id *~ 3
-- 6

-- ((True, "Dudley"), 55.0)
--   & _1 . _2 `undefined` " - the worst"
--   & _2 `undefined` 15
--   & _2 `undefined` 2
--   & _1 . _2 `undefined` map toUpper
--   & _1 . _1 `undefined` False
ex23 = ((True, "Dudley"), 55.0)
  & _1 . _2 <>~ " - the worst"
  & _2 -~ 15
  & _2 //~ 2
  & _1 . _2 %~ map toUpper
  & _1 . _1 &&~ False
-- ((False, "DUDLEY - THE WORST"), 20.0)

-- 3
-- ^.

-- 4

-- (%~) :: Lens s t a b -> (a -> b) -> s -> t
